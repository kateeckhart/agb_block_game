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

use agb::display::object::{DynamicSprite, Object, PaletteVram, SpriteBorrow};
use agb::display::palette16::Palette16;
use agb::display::tiled::{TileFormat, TileSet, TileSetting, TiledMap};
use agb::fixnum::Vector2D;
use agb::input::{Button, ButtonController};
use agb::sync::InitOnce;
use agb::{display, interrupt, sync};
use alloc::vec::Vec;
use core::convert::TryInto;
use core::ops;
use piece_data::{PieceData, PIECE_DATA};
use util::{html_color_to_gba, u16_slice_as_u8};

#[derive(Copy, Clone, Debug)]
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

#[derive(Copy, Clone)]
struct ActivePiece {
    which: Piece,
    rotation: u8,
    pos: Pos,
}

impl ActivePiece {
    fn current_rotation_data(&self) -> &'static Rotation {
        &self.which.data().rotations[self.rotation as usize]
    }

    fn draw(
        &self,
        sprites: &[SpriteBorrow],
        objects: &mut [Object],
        playfield_x: u16,
        playfield_y: u16,
    ) {
        for (block, object) in self
            .current_rotation_data()
            .hitbox
            .iter()
            .zip(objects.iter_mut())
        {
            let screen_x =
                (((block.column + self.pos.column) as i16 * 8) + (playfield_x as i16)) as u16;
            let screen_y = (((block.row + self.pos.row) as i16 * 8) + (playfield_y as i16)) as u16;

            object.set_y(160_u16.wrapping_sub(screen_y + 8));
            object.set_x(screen_x);
            object.set_sprite(sprites[self.which.index()].clone());
        }
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

struct PlayField(pub [[Option<Piece>; 10]; 40]);

struct Game {
    playfield: PlayField,
}

impl Game {
    fn new() -> Self {
        Self {
            playfield: PlayField([[None; 10]; 40]),
        }
    }
}

static GAME: sync::InitOnce<sync::Mutex<Game>> = sync::InitOnce::new();

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

fn gen_global_tileset() -> TileSet<'static> {
    let mut tiles = Vec::with_capacity(16 * 8);
    for i in 0..7 {
        tiles.extend_from_slice(&gen_piece_tile_data(i));
    }
    tiles.resize(tiles.len() + 16, 0x2222);
    TileSet::new(u16_slice_as_u8(tiles.leak()), TileFormat::FourBpp)
}

const SOLID_TILE: u16 = 7;

static GLOBAL_TILESET: InitOnce<TileSet<'static>> = InitOnce::new();

fn get_global_tileset() -> &'static TileSet<'static> {
    GLOBAL_TILESET.get(gen_global_tileset)
}

#[agb::entry]
fn main(mut gba: agb::Gba) -> ! {
    let game_mutex = GAME.get(|| sync::Mutex::new(Game::new()));
    let game = game_mutex.lock();
    let falling_piece_palette = PaletteVram::new(&PIECE_PALETTE).unwrap();
    let mut falling_piece_sprites = Vec::with_capacity(7);
    for i in 0..7 {
        let tile_data = gen_piece_tile_data(i);
        let sprite = DynamicSprite::new(u16_slice_as_u8(&tile_data), display::object::Size::S8x8)
            .to_vram(falling_piece_palette.clone());
        falling_piece_sprites.push(sprite);
    }

    let objects = gba.display.object.get();
    let mut falling_piece_objects = Vec::with_capacity(4);
    for _ in 0..4 {
        falling_piece_objects.push(objects.object(falling_piece_sprites[0].clone()));
    }

    let mut active_piece = ActivePiece {
        which: Piece::I,
        rotation: 0,
        pos: Pos { row: 0, column: 3 },
    };

    let v_blank = interrupt::VBlank::get();

    let mut input = ButtonController::new();

    let (tiled, mut vram) = gba.display.video.tiled0();
    vram.set_background_palettes(GLOBAL_TILE_PALETTES);
    let mut background0 = tiled.background(
        display::Priority::P1,
        display::tiled::RegularBackgroundSize::Background32x32,
    );
    for y in 0..20 {
        let left_pos = Vector2D { x: 9, y };
        let right_pos = Vector2D { x: 20, y };

        let tile_setting = TileSetting::new(SOLID_TILE, false, false, 1);

        background0.set_tile(&mut vram, left_pos, get_global_tileset(), tile_setting);
        background0.set_tile(&mut vram, right_pos, get_global_tileset(), tile_setting);
    }
    background0.commit(&mut vram);
    background0.show();

    loop {
        input.update();

        if input.is_just_pressed(Button::A) {
            active_piece.clockwise_rotate(&game.playfield);
        } else if input.is_just_pressed(Button::B) {
            active_piece.counter_clockwise_rotate(&game.playfield);
        } else if input.is_just_pressed(Button::L) {
            active_piece.which =
                Piece::from_index(active_piece.which.index().checked_sub(1).unwrap_or(6));
            active_piece.rotation = 0;
        } else if input.is_just_pressed(Button::R) {
            active_piece.which = Piece::from_index((active_piece.which.index() + 1) % 7);
            active_piece.rotation = 0;
        } else if input.is_just_pressed(Button::LEFT) {
            active_piece.shift(&game.playfield, Pos { row: 0, column: -1 });
        } else if input.is_just_pressed(Button::RIGHT) {
            active_piece.shift(&game.playfield, Pos { row: 0, column: 1 });
        } else if input.is_just_pressed(Button::UP) {
            active_piece.shift(&game.playfield, Pos { row: 1, column: 0 });
        } else if input.is_just_pressed(Button::DOWN) {
            active_piece.shift(&game.playfield, Pos { row: -1, column: 0 });
        }

        active_piece.draw(&falling_piece_sprites, &mut falling_piece_objects, 80, 0);

        v_blank.wait_for_vblank();
        objects.commit();
    }
}
