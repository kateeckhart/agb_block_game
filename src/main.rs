#![no_std]
#![no_main]
// This is required to allow writing tests
#![cfg_attr(test, feature(custom_test_frameworks))]
#![cfg_attr(test, reexport_test_harness_main = "test_main")]
#![cfg_attr(test, test_runner(agb::test_runner::test_runner))]
#![feature(const_mut_refs)]
#![feature(const_cmp)]

extern crate alloc;

mod piece_data;
mod util;

use agb::display::object::{DynamicSprite, Object, ObjectController, PaletteVram, SpriteBorrow};
use agb::display::palette16::Palette16;
use agb::display::tiled::{
    MapLoan, RegularMap, TileFormat, TileSet, TileSetting, Tiled0, TiledMap, VRamManager,
};
use agb::fixnum::Vector2D;
use agb::input::{Button, ButtonController};
use agb::sync::InitOnce;
use agb::{display, interrupt};
use agb::rng::RandomNumberGenerator;
use alloc::vec::Vec;
use core::cmp::min;
use core::convert::TryInto;
use core::mem::MaybeUninit;
use core::ops;
use piece_data::{PieceData, PIECE_DATA};
use util::{html_color_to_gba, u16_slice_as_u8};

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
enum Piece {
    I,
    O,
    L,
    J,
    T,
    S,
    Z,
}

impl Piece {
    fn index(self) -> usize {
        match self {
            Piece::I => 0,
            Piece::O => 1,
            Piece::L => 2,
            Piece::J => 3,
            Piece::T => 4,
            Piece::S => 5,
            Piece::Z => 6,
        }
    }

    fn from_index(idx: usize) -> Self {
        match idx {
            0 => Piece::I,
            1 => Piece::O,
            2 => Piece::L,
            3 => Piece::J,
            4 => Piece::T,
            5 => Piece::S,
            6 => Piece::Z,
            _ => panic!("Invalid piece index"),
        }
    }

    fn data(self) -> &'static PieceData {
        &PIECE_DATA[self.index()]
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct ActivePieceData {
    which: Piece,
    rotation: u8,
    pos: Pos,
}

impl ActivePieceData {
    fn new(rng: &mut RandomNumberGenerator) -> ActivePieceData {
        let which = Piece::from_index((rng.gen() as usize) % 7);
        let row = if which == Piece::I {
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
            if field.0.get(row_index)?.get(column_index)?.is_some() {
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
    objects: Vec<Object<'a>>,
}

impl<'a> ActivePiece<'a> {
    fn new(oam: &'a ObjectController, rng: &mut RandomNumberGenerator) -> ActivePiece<'a> {
        let mut falling_piece_objects = Vec::with_capacity(4);
        let falling_piece_sprites = get_falling_piece_sprites();
        for _ in 0..4 {
            falling_piece_objects.push(oam.object(falling_piece_sprites[0].clone()));
        }
        let mut ret = ActivePiece {
            data: ActivePieceData::new(rng),
            objects: falling_piece_objects,
        };

        ret.update_color();
        ret
    }

    fn update_color(&mut self) {
        let falling_piece_sprites = get_falling_piece_sprites();
        for obj in &mut self.objects {
            obj.set_sprite(falling_piece_sprites[self.data.which.index()].clone());
        }
    }

    fn reroll(&mut self, rng: &mut RandomNumberGenerator) {
        self.data = ActivePieceData::new(rng);
        self.update_color();
    }

    fn draw(&mut self, playfield_x: u16, playfield_y: u16) {
        for (block, object) in self
            .data
            .current_rotation_data()
            .hitbox
            .iter()
            .zip(self.objects.iter_mut())
        {
            let sprites = get_falling_piece_sprites();
            let screen_x =
                (((block.column + self.data.pos.column) as i16 * 8) + (playfield_x as i16)) as u16;
            let screen_y =
                (((block.row + self.data.pos.row) as i16 * 8) + (playfield_y as i16)) as u16;

            object.set_y(160_u16.wrapping_sub(screen_y + 8));
            object.set_x(screen_x);
            object.set_sprite(sprites[self.data.which.index()].clone());
        }
    }
}

struct PlayField(pub [[Option<Piece>; 10]; 40]);

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
    fn new(oam: &'a ObjectController, background: &'a Tiled0, rng: &mut RandomNumberGenerator) -> Self {
        Self {
            active_piece: ActivePiece::new(oam, rng),
            locked_piece_background: background.background(
                display::Priority::P1,
                display::tiled::RegularBackgroundSize::Background32x32,
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

            let tile_setting = TileSetting::new(SOLID_TILE, false, false, 1);

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
                let screen_pos = Vector2D { x: (10 + j) as u16, y: (19 - i) as u16 };
                let tile_id = if let Some(blk) = block {
                    blk.index() as u16
                } else {
                    BLANK_TILE
                };

                let tile_setting = TileSetting::new(tile_id, false, false, 0);
                self.locked_piece_background.set_tile(vram, screen_pos, get_global_tileset(), tile_setting)
            }
        }
        self.locked_piece_background.show();
    }

    fn sprite_draw(&mut self, vram: &mut VRamManager) {
        self.active_piece.draw(80, 0);
    }

    fn commit_backgrounds(&mut self, vram: &mut VRamManager) {
        self.locked_piece_background.commit(vram);
    }
}

enum GameMode<'a> {
    Blank,
    Playing(PlayMode<'a>),
}

struct CommonGameData {
    playfield: PlayField,
    debug_active: bool,
    dirty_screen: bool,
    level: u8,
    input: ButtonController,
    rng: RandomNumberGenerator,
}

struct Game<'a> {
    data: CommonGameData,
    mode: GameMode<'a>,
}

impl<'a> Game<'a> {
    fn redraw(&mut self, vram: &mut VRamManager) {
        match self.mode {
            GameMode::Playing(ref mut play) => play.redraw(&self.data, vram),
            _ => (),
        }
    }

    fn sprite_draw(&mut self, vram: &mut VRamManager) {
        match self.mode {
            GameMode::Playing(ref mut play) => play.sprite_draw(vram),
            _ => (),
        }
    }

    fn play_tick(&mut self, vram: &mut VRamManager) {
        let mut should_lock = false;
        let mut play = match self.mode {
            GameMode::Playing(ref mut play) => play,
                _ => panic!(),
        };
        let begin_piece = play.active_piece.data;

        if self.data.input.is_just_pressed(Button::A) {
            play.active_piece.data.clockwise_rotate(&self.data.playfield);
        } else if self.data.input.is_just_pressed(Button::B) {
            play.active_piece.data.counter_clockwise_rotate(&self.data.playfield);
        } else if self.data.input.is_just_pressed(Button::LEFT) {
            play.active_piece.data.shift(&self.data.playfield, Pos { row: 0, column: -1 });
        } else if self.data.input.is_just_pressed(Button::RIGHT) {
            play.active_piece.data.shift(&self.data.playfield, Pos { row: 0, column: 1 });
        }

        if self.data.debug_active {
            if self.data.input.is_just_pressed(Button::UP) {
                play.active_piece.data.shift(&self.data.playfield, Pos { row: 1, column: 0 });
            } else if self.data.input.is_just_pressed(Button::L) {
                play.active_piece.data.which =
                    Piece::from_index(play.active_piece.data.which.index().checked_sub(1).unwrap_or(6));
                play.active_piece.data.rotation = 0;
            } else if self.data.input.is_just_pressed(Button::R) {
                play.active_piece.data.which = Piece::from_index((play.active_piece.data.which.index() + 1) % 7);
                play.active_piece.data.rotation = 0;
            } else if self.data.input.is_just_pressed(Button::DOWN) {
                should_lock = !play.active_piece.data.shift(&self.data.playfield, Pos { row: -1, column: 0 });
            }
        } else {
            if self.data.input.is_pressed(Button::DOWN) {
                play.gravity_timer = min(play.gravity_timer, 2);
            } else if self.data.input.is_just_pressed(Button::UP) {
                while play.active_piece.data.shift(&self.data.playfield, Pos { row: -1, column: 0 }) {}
                should_lock = true;
                play.lock_reset_count = 0;
                play.lock_delay = 0;
            }

            if play.gravity_timer == 0 {
                should_lock = !play.active_piece.data.shift(&self.data.playfield, Pos { row: -1, column: 0 });
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
                self.data.playfield.0[block_pos.row as usize][block_pos.column as usize] = Some(play.active_piece.data.which);
                self.data.dirty_screen = true;
            }
            
            let mut new_playfield = Vec::with_capacity(40);

            for row in self.data.playfield.0 {
                if row.iter().any(Option::is_none) {
                    new_playfield.push(row);
                }
            }

            new_playfield.resize(40, [None; 10]);
            self.data.playfield.0.copy_from_slice(&new_playfield);

            play.active_piece.reroll(&mut self.data.rng);
            play.gravity_timer = GRAVITY_TIMER;
            play.lock_delay = LOCK_DELAY_LEN;
            play.lock_reset_count = LOCK_RESET_COUNT;
        } else if should_lock {
            play.lock_delay -= 1;
        }

    }

    fn tick(&mut self, vram: &mut VRamManager) {
        match self.mode {
            GameMode::Playing(_) => self.play_tick(vram),
            _ => (),
        }
    }

    fn commit_backgrounds(&mut self, vram: &mut VRamManager) {
        match self.mode {
            GameMode::Playing(ref mut play) => play.commit_backgrounds(vram),
            _ => (),
        }
    }
}

impl<'a> Game<'a> {
    fn new(oam: &'a ObjectController, background: &'a Tiled0) -> Self {
        let mut rng = RandomNumberGenerator::new();
        let mode = GameMode::Playing(PlayMode::new(oam, background, &mut rng));
        Self {
            data: CommonGameData {
                input: ButtonController::new(),
                playfield: PlayField([[None; 10]; 40]),
                debug_active: false,
                dirty_screen: true,
                level: 0,
                rng,
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

static PIECE_PALETTE: Palette16 = gen_piece_palette();

const fn gen_global_tile_palettes() -> [Palette16; 2] {
    const NULL_PALETTE: Palette16 = Palette16::new([0; 16]);
    let mut ret = [NULL_PALETTE; 2];
    ret[0] = gen_piece_palette();

    let palette = [0x0, 0x0, 0xffffff];
    let mut gba_palette = [0; 16];

    let mut i = 0;
    while i < palette.len() {
        gba_palette[i] = html_color_to_gba(palette[i]);
        i += 1;
    }
    ret[1] = Palette16::new(gba_palette);

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
    fn gen_global_tileset() -> TileSet<'static> {
        let mut tiles = Vec::with_capacity(16 * 9);
        for i in 0..7 {
            tiles.extend_from_slice(&gen_piece_tile_data(i));
        }
        tiles.resize(tiles.len() + 16, 0x2222);
        tiles.resize(tiles.len() + 16, 0x0);
        TileSet::new(u16_slice_as_u8(tiles.leak()), TileFormat::FourBpp)
    }

    GLOBAL_TILESET.get(gen_global_tileset)
}

//Don't call from irq lul
fn get_falling_piece_sprites() -> &'static [SpriteBorrow] {
    static mut FALLING_PIECE_SPRITES: InitOnce<Vec<SpriteBorrow>> = InitOnce::new();
    fn gen_falling_piece_sprites() -> Vec<SpriteBorrow> {
        let falling_piece_palette = PaletteVram::new(&PIECE_PALETTE).unwrap();
        let mut falling_piece_sprites = Vec::with_capacity(7);
        for i in 0..7 {
            let tile_data = gen_piece_tile_data(i);
            let sprite =
                DynamicSprite::new(u16_slice_as_u8(&tile_data), display::object::Size::S8x8)
                    .to_vram(falling_piece_palette.clone());
            falling_piece_sprites.push(sprite);
        }
        falling_piece_sprites
    }

    unsafe { FALLING_PIECE_SPRITES.get(gen_falling_piece_sprites) }
}

#[agb::entry]
fn main(mut gba: agb::Gba) -> ! {
    let objects = gba.display.object.get();

    let v_blank = interrupt::VBlank::get();

    let (tiled, mut vram) = gba.display.video.tiled0();
    vram.set_background_palettes(GLOBAL_TILE_PALETTES);

    fn assign_game_lifetimes<'a>(
        game: &'a mut MaybeUninit<Game<'static>>,
        _object: &'a agb::display::object::ObjectController,
        _background: &'a agb::display::tiled::Tiled0,
    ) -> &'a mut MaybeUninit<Game<'a>> {
        unsafe { core::mem::transmute(game) }
    }

    let mut game = unsafe { assign_game_lifetimes(&mut GAME, &objects, &tiled) }.write(Game::new(&objects, &tiled));

    let mut debug_indicator = objects.object(get_falling_piece_sprites()[0].clone());
    debug_indicator.set_x(0);
    debug_indicator.set_y(0);
    debug_indicator.hide();

    loop {
        game.data.rng.gen();
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
