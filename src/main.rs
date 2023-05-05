#![no_std]
#![no_main]
// This is required to allow writing tests
#![cfg_attr(test, feature(custom_test_frameworks))]
#![cfg_attr(test, reexport_test_harness_main = "test_main")]
#![cfg_attr(test, test_runner(agb::test_runner::test_runner))]
#![feature(const_mut_refs)]
#![feature(const_cmp)]

mod piece_data;
mod util;

use agb::display::font::{Font, TextRenderer};
use agb::display::object::{DynamicSprite, OamManaged, Object, PaletteVram, SpriteVram};
use agb::display::palette16::Palette16;
use agb::display::tiled::{
    MapLoan, RegularMap, TileFormat, TileSet, TileSetting, Tiled0, TiledMap, VRamManager,
};
use agb::fixnum::Vector2D;
use agb::include_font;
use agb::input::{Button, ButtonController};
use agb::sync::InitOnce;
use agb::{display, interrupt};
use core::cmp::min;
use core::convert::TryInto;
use core::fmt::Write;
use core::mem::MaybeUninit;
use core::ops;
use piece_data::{PieceData, PIECE_DATA};
use rand::prelude::*;
use rand_chacha::ChaCha8Rng;
use smallvec::{smallvec_inline, SmallVec};
use util::{html_color_to_gba, u16_slice_as_u8};

static BIG_FONT: Font = include_font!("data/third_party/noto/NotoSerif-Regular.ttf", 24);
static MAIN_FONT: Font = include_font!("data/third_party/noto/NotoSerif-Regular.ttf", 12);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct Pos {
    row: i8,
    column: i8,
}

impl ops::Add for Pos {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self {
            row: self.row + rhs.row,
            column: self.column + rhs.column,
        }
    }
}

impl ops::AddAssign for Pos {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

#[derive(Debug)]
struct Rotation {
    hitbox: [Pos; 4],
    clockwise_wallkick: [Pos; 5],
    counter_clockwise_wallkick: [Pos; 5],
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum BasePiece {
    I,
    O,
    L,
    J,
    T,
    S,
    Z,
}

impl BasePiece {
    fn index(self) -> usize {
        match self {
            BasePiece::I => 0,
            BasePiece::O => 1,
            BasePiece::L => 2,
            BasePiece::J => 3,
            BasePiece::T => 4,
            BasePiece::S => 5,
            BasePiece::Z => 6,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum LockedPiece {
    Empty,
    Base(BasePiece),
    Garbage,
}

impl LockedPiece {
    fn tile_index(self) -> u16 {
        match self {
            LockedPiece::Empty => BLANK_TILE,
            LockedPiece::Base(piece) => piece.index() as u16,
            LockedPiece::Garbage => 1,
        }
    }

    fn pal_index(self) -> u8 {
        match self {
            LockedPiece::Empty => 0,
            LockedPiece::Base(_) => 1,
            LockedPiece::Garbage => 2,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum ActivePieceType {
    Base(BasePiece),
    Ghost(BasePiece),
}

const fn gen_full_bag() -> [ActivePieceType; 7] {
    use ActivePieceType::Base as B;
    use BasePiece::*;

    [B(I), B(O), B(L), B(J), B(T), B(S), B(Z)]
}

const FULL_BAG: [ActivePieceType; 7] = gen_full_bag();

impl ActivePieceType {
    fn base(self) -> BasePiece {
        match self {
            ActivePieceType::Base(piece) | ActivePieceType::Ghost(piece) => piece,
        }
    }

    fn lock(self) -> LockedPiece {
        LockedPiece::Base(self.base())
    }

    fn sprite(self) -> &'static SpriteVram {
        let sprites = get_falling_piece_sprites();
        match self {
            ActivePieceType::Base(piece) => &sprites[piece.index()],
            ActivePieceType::Ghost(_) => &sprites[7],
        }
    }

    fn debug_next(self) -> Self {
        ActivePieceType::Base(match self.base() {
            BasePiece::I => BasePiece::O,
            BasePiece::O => BasePiece::L,
            BasePiece::L => BasePiece::J,
            BasePiece::J => BasePiece::T,
            BasePiece::T => BasePiece::S,
            BasePiece::S => BasePiece::Z,
            BasePiece::Z => BasePiece::I,
        })
    }

    fn debug_prev(self) -> Self {
        ActivePieceType::Base(match self.base() {
            BasePiece::I => BasePiece::Z,
            BasePiece::O => BasePiece::I,
            BasePiece::L => BasePiece::O,
            BasePiece::J => BasePiece::L,
            BasePiece::T => BasePiece::J,
            BasePiece::S => BasePiece::T,
            BasePiece::Z => BasePiece::S,
        })
    }

    fn data(self) -> &'static PieceData {
        &PIECE_DATA[self.base().index()]
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct ActivePieceData {
    which: ActivePieceType,
    rotation: u8,
    pos: Pos,
}

impl ActivePieceData {
    fn new(bag: &mut PieceBag) -> ActivePieceData {
        let which = bag.next();
        let row = if which == ActivePieceType::Base(BasePiece::I) {
            17
        } else {
            18
        };
        ActivePieceData {
            which,
            rotation: 0,
            pos: Pos { row, column: 3 },
        }
    }

    fn current_rotation_data(&self) -> &'static Rotation {
        &self.which.data().rotations[self.rotation as usize]
    }

    fn blocked_inner(&self, field: &PlayField) -> Option<()> {
        for block in self.current_rotation_data().hitbox {
            let block_pos = self.pos + block;
            let row_index: usize = block_pos.row.try_into().ok()?;
            let column_index: usize = block_pos.column.try_into().ok()?;
            if *field.0.get(row_index)?.get(column_index)? != LockedPiece::Empty {
                return None;
            }
        }
        Some(())
    }

    fn blocked(&self, field: &PlayField) -> bool {
        self.blocked_inner(field).is_none()
    }

    fn shift(&mut self, field: &PlayField, offset: Pos) -> bool {
        let mut new_piece = *self;
        new_piece.pos += offset;

        if !new_piece.blocked(field) {
            *self = new_piece;
            true
        } else {
            false
        }
    }

    fn clockwise_rotate(&mut self, field: &PlayField) -> bool {
        let mut new_piece = *self;
        new_piece.rotation = (new_piece.rotation + 1) % 4;

        for wall_kick in self.current_rotation_data().clockwise_wallkick {
            if new_piece.shift(field, wall_kick) {
                *self = new_piece;
                return true;
            }
        }

        false
    }

    fn counter_clockwise_rotate(&mut self, field: &PlayField) -> bool {
        let mut new_piece = *self;
        new_piece.rotation = new_piece.rotation.checked_sub(1).unwrap_or(3);

        for wall_kick in self.current_rotation_data().counter_clockwise_wallkick {
            if new_piece.shift(field, wall_kick) {
                *self = new_piece;
                return true;
            }
        }

        false
    }
}

struct ActivePiece<'a> {
    data: ActivePieceData,
    objects: SmallVec<[Object<'a>; 4]>,
}

impl<'a> ActivePiece<'a> {
    fn new(oam: &'a OamManaged, bag: &mut PieceBag) -> ActivePiece<'a> {
        let mut falling_piece_objects = SmallVec::new();
        let falling_piece_sprites = get_falling_piece_sprites();
        for _ in 0..4 {
            falling_piece_objects.push(oam.object(falling_piece_sprites[0].clone()));
        }
        ActivePiece {
            data: ActivePieceData::new(bag),
            objects: falling_piece_objects,
        }
    }

    fn reroll(&mut self, bag: &mut PieceBag) {
        self.data = ActivePieceData::new(bag);
    }

    fn draw(&mut self, playfield_x: u16, playfield_y: u16) {
        for (block, object) in self
            .data
            .current_rotation_data()
            .hitbox
            .iter()
            .zip(self.objects.iter_mut())
        {
            let screen_x =
                (((block.column + self.data.pos.column) as i16 * 8) + (playfield_x as i16)) as u16;
            let screen_y =
                (((block.row + self.data.pos.row) as i16 * 8) + (playfield_y as i16)) as u16;

            object.set_y(160_u16.wrapping_sub(screen_y + 8));
            object.set_x(screen_x);
            object.set_sprite(self.data.which.sprite().clone());
            object.show();
        }
    }
}

struct PlayField(pub SmallVec<[[LockedPiece; 10]; 40]>);

const GRAVITY_TIMER: u16 = 60 * 3;
const LOCK_DELAY_LEN: u16 = 60;
const LOCK_RESET_COUNT: u8 = 20;

struct PlayMode<'a> {
    active_piece: ActivePiece<'a>,
    locked_piece_background: MapLoan<'a, RegularMap>,
    gravity_timer: u16,
    lock_delay: u16,
    lock_reset_count: u8,
}

impl<'a> PlayMode<'a> {
    fn new(oam: &'a OamManaged, background: &'a Tiled0, bag: &mut PieceBag) -> Self {
        Self {
            active_piece: ActivePiece::new(oam, bag),
            locked_piece_background: background.background(
                display::Priority::P1,
                display::tiled::RegularBackgroundSize::Background32x32,
                TileFormat::FourBpp,
            ),
            gravity_timer: GRAVITY_TIMER,
            lock_delay: LOCK_DELAY_LEN,
            lock_reset_count: LOCK_RESET_COUNT,
        }
    }

    fn redraw(&mut self, data: &CommonGameData, vram: &mut VRamManager) {
        for y in 0..20 {
            let left_pos = Vector2D { x: 9, y };
            let right_pos = Vector2D { x: 20, y };

            let tile_setting = TileSetting::new(SOLID_TILE, false, false, 0);

            self.locked_piece_background.set_tile(
                vram,
                left_pos,
                get_global_tileset(),
                tile_setting,
            );
            self.locked_piece_background.set_tile(
                vram,
                right_pos,
                get_global_tileset(),
                tile_setting,
            );
        }
        for (i, row) in data.playfield.0[..20].iter().enumerate() {
            for (j, block) in row.iter().enumerate() {
                let screen_pos = Vector2D {
                    x: (10 + j) as u16,
                    y: (19 - i) as u16,
                };

                let tile_setting =
                    TileSetting::new(block.tile_index(), false, false, block.pal_index());
                self.locked_piece_background.set_tile(
                    vram,
                    screen_pos,
                    get_global_tileset(),
                    tile_setting,
                )
            }
        }
        self.locked_piece_background.show();
    }

    fn sprite_draw(&mut self, _: &mut VRamManager) {
        self.active_piece.draw(80, 0);
    }

    fn commit_backgrounds(&mut self, vram: &mut VRamManager) {
        self.locked_piece_background.commit(vram);
    }
}

enum GameMode<'a> {
    Blank,
    Playing(PlayMode<'a>),
    Title(TitleMode<'a>),
}

struct TitleMode<'a> {
    background: MapLoan<'a, RegularMap>,
    text_renderers: [TextRenderer<'static>; 3],
}

impl<'a> TitleMode<'a> {
    fn new(background_man: &'a Tiled0) -> Self {
        let background = background_man.background(
            display::Priority::P1,
            display::tiled::RegularBackgroundSize::Background32x32,
            TileFormat::FourBpp,
        );

        let text_renderer_a = BIG_FONT.render_text(Vector2D { x: 0, y: 0 });

        let text_renderer_b = MAIN_FONT.render_text(Vector2D { x: 0, y: 8 });

        let text_renderer_c = MAIN_FONT.render_text(Vector2D { x: 0, y: 12 });

        Self {
            background,
            text_renderers: [text_renderer_a, text_renderer_b, text_renderer_c],
        }
    }

    fn redraw(&mut self, _: &CommonGameData, vram: &mut VRamManager) {
        static LINES: &[&str] = &["AGB Block Game", "By Kate \u{1} Eckhart", "Press Start"];
        for (line, renderer) in LINES.iter().zip(&mut self.text_renderers) {
            let mut writer = renderer.writer(2, 1, &mut self.background, vram);
            write!(writer, "{}", line).expect("Title render error");
        }

        self.background.show();
    }

    fn sprite_draw(&mut self, _: &mut VRamManager) {}

    fn commit_backgrounds(&mut self, vram: &mut VRamManager) {
        for line in &mut self.text_renderers {
            line.commit(&mut self.background, vram);
        }
        self.background.commit(vram);
    }
}

struct PieceBag {
    rng: ChaCha8Rng,
    bag: SmallVec<[ActivePieceType; 7]>,
}

impl PieceBag {
    fn new() -> Self {
        Self {
            rng: ChaCha8Rng::from_seed(*include_bytes!("seed.bin")),
            bag: SmallVec::new(),
        }
    }

    fn empty(&mut self) {
        self.bag.truncate(0);
    }

    fn next(&mut self) -> ActivePieceType {
        if let Some(piece) = self.bag.pop() {
            return piece;
        }

        self.bag.extend_from_slice(&FULL_BAG);

        self.bag.shuffle(&mut self.rng);
        self.bag.pop().unwrap()
    }
}

struct CommonGameData<'a> {
    playfield: PlayField,
    debug_active: bool,
    dirty_screen: bool,
    level: u8,
    input: ButtonController,
    bag: PieceBag,
    oam: &'a OamManaged<'a>,
    background_man: &'a Tiled0<'a>,
}

struct Game<'a> {
    data: CommonGameData<'a>,
    mode: GameMode<'a>,
}

impl<'a> Game<'a> {
    fn redraw(&mut self, vram: &mut VRamManager) {
        match self.mode {
            GameMode::Playing(ref mut play) => play.redraw(&self.data, vram),
            GameMode::Title(ref mut title) => title.redraw(&self.data, vram),
            _ => (),
        }
    }

    fn sprite_draw(&mut self, vram: &mut VRamManager) {
        match self.mode {
            GameMode::Playing(ref mut play) => play.sprite_draw(vram),
            GameMode::Title(ref mut title) => title.sprite_draw(vram),
            _ => (),
        }
    }

    fn play_tick(&mut self, _: &mut VRamManager) {
        let mut should_lock = false;
        let mut play = match self.mode {
            GameMode::Playing(ref mut play) => play,
            _ => panic!(),
        };
        let begin_piece = play.active_piece.data;

        if self.data.input.is_just_pressed(Button::A) {
            play.active_piece
                .data
                .clockwise_rotate(&self.data.playfield);
        } else if self.data.input.is_just_pressed(Button::B) {
            play.active_piece
                .data
                .counter_clockwise_rotate(&self.data.playfield);
        } else if self.data.input.is_just_pressed(Button::LEFT) {
            play.active_piece
                .data
                .shift(&self.data.playfield, Pos { row: 0, column: -1 });
        } else if self.data.input.is_just_pressed(Button::RIGHT) {
            play.active_piece
                .data
                .shift(&self.data.playfield, Pos { row: 0, column: 1 });
        }

        if self.data.debug_active {
            if self.data.input.is_just_pressed(Button::UP) {
                play.active_piece
                    .data
                    .shift(&self.data.playfield, Pos { row: 1, column: 0 });
            } else if self.data.input.is_just_pressed(Button::L) {
                play.active_piece.data.which = play.active_piece.data.which.debug_prev();
                play.active_piece.data.rotation = 0;
            } else if self.data.input.is_just_pressed(Button::R) {
                play.active_piece.data.which = play.active_piece.data.which.debug_next();
                play.active_piece.data.rotation = 0;
            } else if self.data.input.is_just_pressed(Button::DOWN) {
                should_lock = !play
                    .active_piece
                    .data
                    .shift(&self.data.playfield, Pos { row: -1, column: 0 });
            }
        } else {
            if self.data.input.is_pressed(Button::DOWN) {
                play.gravity_timer = min(play.gravity_timer, 2);
            } else if self.data.input.is_just_pressed(Button::UP) {
                while play
                    .active_piece
                    .data
                    .shift(&self.data.playfield, Pos { row: -1, column: 0 })
                {}
                should_lock = true;
                play.lock_reset_count = 0;
                play.lock_delay = 0;
            }

            if play.gravity_timer == 0 {
                should_lock = !play
                    .active_piece
                    .data
                    .shift(&self.data.playfield, Pos { row: -1, column: 0 });
                if !should_lock {
                    play.gravity_timer = GRAVITY_TIMER;
                }
            } else {
                play.gravity_timer -= 1;
            }
        }

        if !should_lock {
            play.lock_delay = LOCK_DELAY_LEN;
        }

        if should_lock && begin_piece != play.active_piece.data && play.lock_reset_count > 0 {
            play.lock_reset_count -= 1;
            play.lock_delay = LOCK_DELAY_LEN;
        }

        if should_lock && play.lock_delay == 0 {
            for block in play.active_piece.data.current_rotation_data().hitbox {
                let block_pos = play.active_piece.data.pos + block;
                self.data.playfield.0[block_pos.row as usize][block_pos.column as usize] =
                    play.active_piece.data.which.lock();
                self.data.dirty_screen = true;
            }

            self.data
                .playfield
                .0
                .retain(|row| row.iter().any(|x| *x == LockedPiece::Empty));

            self.data.playfield.0.resize(40, [LockedPiece::Empty; 10]);

            play.active_piece.reroll(&mut self.data.bag);
            play.gravity_timer = GRAVITY_TIMER;
            play.lock_delay = LOCK_DELAY_LEN;
            play.lock_reset_count = LOCK_RESET_COUNT;
        } else if should_lock {
            play.lock_delay -= 1;
        }
    }

    fn title_tick(&mut self, _: &mut VRamManager) {
        if self.data.input.is_just_pressed(Button::START) {
            self.mode = GameMode::Blank;
            self.data.bag.empty();
            self.mode = GameMode::Playing(PlayMode::new(
                self.data.oam,
                self.data.background_man,
                &mut self.data.bag,
            ));
            self.data.dirty_screen = true;
        }
    }

    fn tick(&mut self, vram: &mut VRamManager) {
        match self.mode {
            GameMode::Playing(_) => self.play_tick(vram),
            GameMode::Title(_) => self.title_tick(vram),
            _ => (),
        }
    }

    fn commit_backgrounds(&mut self, vram: &mut VRamManager) {
        match self.mode {
            GameMode::Title(ref mut title) => title.commit_backgrounds(vram),
            GameMode::Playing(ref mut play) => play.commit_backgrounds(vram),
            _ => (),
        }
    }
}

impl<'a> Game<'a> {
    fn new(oam: &'a OamManaged<'a>, background_man: &'a Tiled0<'a>) -> Self {
        let bag = PieceBag::new();
        let mode = GameMode::Title(TitleMode::new(background_man));
        Self {
            data: CommonGameData {
                oam,
                background_man,
                input: ButtonController::new(),
                playfield: PlayField(smallvec_inline![[LockedPiece::Empty; 10]; 40]),
                debug_active: false,
                dirty_screen: true,
                level: 0,
                bag,
            },
            mode,
        }
    }
}

static mut GAME: MaybeUninit<Game<'static>> = MaybeUninit::uninit();

const fn gen_piece_palette() -> Palette16 {
    let palette = [
        0x0, 0xf52f11, 0x99200e, 0x37d60b, 0x195c06, 0x1bd6e3, 0x10676e, 0xb515e6, 0x4d0a61,
        0xe810c0, 0x631d57, 0x091ced, 0x0a126b, 0xed940e, 0x472c04,
    ];

    let mut compressed_colors = [0; 16];

    let mut i = 0;
    while i < palette.len() {
        compressed_colors[i] = html_color_to_gba(palette[i]);
        i += 1;
    }

    Palette16::new(compressed_colors)
}

static MAIN_PIECE_PALETTE: Palette16 = gen_piece_palette();

const fn gen_aux_piece_palette() -> Palette16 {
    let palette = [0x0, 0x0, 0x3226b, 0xc5c5c5, 0x939393];

    let mut compressed_colors = [0; 16];

    let mut i = 0;
    while i < palette.len() {
        compressed_colors[i] = html_color_to_gba(palette[i]);
        i += 1;
    }

    Palette16::new(compressed_colors)
}

static AUX_PIECE_PALETTE: Palette16 = gen_aux_piece_palette();

const fn gen_global_tile_palettes() -> [Palette16; 3] {
    const NULL_PALETTE: Palette16 = Palette16::new([0; 16]);
    let mut ret = [NULL_PALETTE; 3];

    let palette = [0x0, 0x0, 0xffffff, 0xffffff];
    let mut gba_palette = [0; 16];

    let mut i = 0;
    while i < palette.len() {
        gba_palette[i] = html_color_to_gba(palette[i]);
        i += 1;
    }

    ret[0] = Palette16::new(gba_palette);
    ret[1] = gen_piece_palette();
    ret[2] = gen_aux_piece_palette();

    ret
}

static GLOBAL_TILE_PALETTES: &[Palette16] = &gen_global_tile_palettes();

fn gen_piece_tile_data(index: u8) -> [u16; 16] {
    let main_color = ((index as u16) * 2) + 1;
    let border_color = main_color + 1;
    let mut tiles = [0; 16];

    for y in 0..8 {
        for x in 0..8 {
            let color = if (1..=6).contains(&x) && (1..=6).contains(&y) {
                main_color
            } else {
                border_color
            };

            let pixel_num = x + y * 8;
            let word_num = pixel_num / 4;
            let bit_num = (pixel_num % 4) * 4;
            tiles[word_num] |= color << bit_num;
        }
    }

    tiles
}

const SOLID_TILE: u16 = 7;
const BLANK_TILE: u16 = 8;

fn get_global_tileset() -> &'static TileSet<'static> {
    static GLOBAL_TILESET: InitOnce<TileSet<'static>> = InitOnce::new();
    static mut TILESTORE: SmallVec<[u16; 16 * 9]> = SmallVec::new_const();
    fn gen_global_tileset() -> TileSet<'static> {
        let tiles = unsafe { &mut TILESTORE };
        for i in 0..7 {
            tiles.extend_from_slice(&gen_piece_tile_data(i));
        }
        tiles.resize(tiles.len() + 16, 0x3333);
        tiles.resize(tiles.len() + 16, 0x0);
        TileSet::new(u16_slice_as_u8(tiles), TileFormat::FourBpp)
    }

    GLOBAL_TILESET.get(gen_global_tileset)
}

//Don't call from irq lul
fn get_falling_piece_sprites() -> &'static [SpriteVram] {
    static mut FALLING_PIECE_SPRITES: InitOnce<SmallVec<[SpriteVram; 8]>> = InitOnce::new();
    fn gen_falling_piece_sprites() -> SmallVec<[SpriteVram; 8]> {
        let falling_piece_palette = PaletteVram::new(&MAIN_PIECE_PALETTE).unwrap();
        let mut falling_piece_sprites = SmallVec::new();
        for i in 0..7 {
            let tile_data = gen_piece_tile_data(i);
            let sprite =
                DynamicSprite::new(u16_slice_as_u8(&tile_data), display::object::Size::S8x8)
                    .to_vram(falling_piece_palette.clone());
            falling_piece_sprites.push(sprite);
        }
        let falling_piece_palette2 = PaletteVram::new(&AUX_PIECE_PALETTE).unwrap();
        let ghost_tile_data = gen_piece_tile_data(0);
        let ghost_tile = DynamicSprite::new(
            u16_slice_as_u8(&ghost_tile_data),
            display::object::Size::S8x8,
        )
        .to_vram(falling_piece_palette2);
        falling_piece_sprites.push(ghost_tile);

        falling_piece_sprites
    }

    unsafe { FALLING_PIECE_SPRITES.get(gen_falling_piece_sprites) }
}

#[agb::entry]
fn main(mut gba: agb::Gba) -> ! {
    let objects = gba.display.object.get_managed();

    let v_blank = interrupt::VBlank::get();

    let (tiled, mut vram) = gba.display.video.tiled0();
    vram.set_background_palettes(GLOBAL_TILE_PALETTES);

    fn assign_game_lifetimes<'a>(
        game: &'a mut MaybeUninit<Game<'static>>,
    ) -> &'a mut MaybeUninit<Game<'a>> {
        unsafe { core::mem::transmute(game) }
    }

    let mut game = unsafe { assign_game_lifetimes(&mut GAME) }.write(Game::new(&objects, &tiled));

    let mut debug_indicator = objects.object(get_falling_piece_sprites()[0].clone());
    debug_indicator.set_x(0);
    debug_indicator.set_y(0);
    debug_indicator.hide();

    loop {
        game.data.bag.rng.next_u32();
        game.data.input.update();
        if game.data.input.is_just_pressed(Button::SELECT) {
            game.data.debug_active = !game.data.debug_active;
            if game.data.debug_active {
                debug_indicator.show();
            } else {
                debug_indicator.hide();
            }
        }

        game.tick(&mut vram);

        if game.data.dirty_screen {
            game.data.dirty_screen = false;
            game.redraw(&mut vram);
        }

        game.sprite_draw(&mut vram);

        v_blank.wait_for_vblank();
        objects.commit();
        game.commit_backgrounds(&mut vram);
    }
}
