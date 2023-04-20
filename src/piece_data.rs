use crate::{Pos, Rotation};

#[derive(Debug)]
pub(crate) struct PieceData {
    pub rotations: [Rotation; 4],
}

const NULL_POS: Pos = Pos { row: 0, column: 0 };

const NULL_ROTATION: Rotation = Rotation {
    hitbox: [NULL_POS; 4],
    clockwise_wallkick: [NULL_POS; 5],
    counter_clockwise_wallkick: [NULL_POS; 5],
};

const fn gen_o_rotation() -> [Rotation; 4] {
    let mut ret = [NULL_ROTATION; 4];

    let mut i = 0;
    while i < 4 {
        ret[i].hitbox[0] = Pos { row: 1, column: 1 };
        ret[i].hitbox[1] = Pos { row: 1, column: 2 };
        ret[i].hitbox[2] = Pos { row: 2, column: 1 };
        ret[i].hitbox[3] = Pos { row: 2, column: 2 };

        i += 1;
    }
    ret
}

const O_ROTATION: [Rotation; 4] = gen_o_rotation();

const fn calc_counter_clockwise_wallkick(rotate_data: &mut [Rotation; 4]) {
    let mut i = 0;
    while i < 4 {
        let last_piece = if i == 0 { 3 } else { i - 1 };

        let mut counter_wallkick = rotate_data[last_piece].clockwise_wallkick;
        let mut j = 0;
        while j < counter_wallkick.len() {
            counter_wallkick[j].row = -counter_wallkick[j].row;
            counter_wallkick[j].column = -counter_wallkick[j].column;
            j += 1;
        }

        rotate_data[i].counter_clockwise_wallkick = counter_wallkick;

        i += 1;
    }
}

const fn gen_i_rotation() -> [Rotation; 4] {
    let mut ret = [NULL_ROTATION; 4];
    let mut i = 0;
    while i < 4 {
        ret[0].hitbox[i] = Pos {
            row: 2,
            column: i as i8,
        };
        ret[2].hitbox[i] = Pos {
            row: 1,
            column: i as i8,
        };

        ret[1].hitbox[i] = Pos {
            row: i as i8,
            column: 2,
        };
        ret[3].hitbox[i] = Pos {
            row: i as i8,
            column: 1,
        };

        i += 1;
    }

    const fn p(column: i8, row: i8) -> Pos {
        Pos { row, column }
    }

    ret[0].clockwise_wallkick = [p(0, 0), p(-2, 0), p(1, 0), p(-2, -1), p(1, 2)];

    ret[1].clockwise_wallkick = [p(0, 0), p(-1, 0), p(2, 0), p(-1, 2), p(2, -1)];

    ret[2].clockwise_wallkick = [p(0, 0), p(2, 0), p(-1, 0), p(2, 1), p(-1, -2)];

    ret[3].clockwise_wallkick = [p(0, 0), p(1, 0), p(-2, 0), p(1, -2), p(-2, 1)];

    calc_counter_clockwise_wallkick(&mut ret);

    ret
}

const I_ROTATION: [Rotation; 4] = gen_i_rotation();

const fn gen_wallkick_template() -> [Rotation; 4] {
    let mut ret = [NULL_ROTATION; 4];

    const fn p(column: i8, row: i8) -> Pos {
        Pos { row, column }
    }

    ret[0].clockwise_wallkick = [p(0, 0), p(-1, 0), p(-1, 1), p(0, -2), p(-1, -2)];

    ret[1].clockwise_wallkick = [p(0, 0), p(1, 0), p(1, -1), p(0, 2), p(1, 2)];

    ret[2].clockwise_wallkick = [p(0, 0), p(1, 0), p(1, 1), p(0, -2), p(1, -2)];

    ret[3].clockwise_wallkick = [p(0, 0), p(-1, 0), p(-1, -1), p(0, 2), p(-1, 2)];

    calc_counter_clockwise_wallkick(&mut ret);

    ret
}

const WALLKICK_TEMPLATE: [Rotation; 4] = gen_wallkick_template();

const fn gen_line_three_rotation() -> [Rotation; 4] {
    let mut ret = WALLKICK_TEMPLATE;

    let mut i = 0;
    while i < 3 {
        ret[0].hitbox[i] = Pos {
            row: 1,
            column: i as i8,
        };
        ret[2].hitbox[i] = Pos {
            row: 1,
            column: i as i8,
        };

        ret[1].hitbox[i] = Pos {
            row: i as i8,
            column: 1,
        };
        ret[3].hitbox[i] = Pos {
            row: i as i8,
            column: 1,
        };
        i += 1;
    }

    ret
}

const LINE_THREE_ROTATION: [Rotation; 4] = gen_line_three_rotation();

const fn gen_l_rotation() -> [Rotation; 4] {
    let mut ret = LINE_THREE_ROTATION;

    ret[0].hitbox[3] = Pos { row: 2, column: 2 };
    ret[1].hitbox[3] = Pos { row: 0, column: 2 };
    ret[2].hitbox[3] = Pos { row: 0, column: 0 };
    ret[3].hitbox[3] = Pos { row: 2, column: 0 };

    ret
}

const L_ROTATION: [Rotation; 4] = gen_l_rotation();

const fn gen_j_rotation() -> [Rotation; 4] {
    let mut ret = LINE_THREE_ROTATION;

    ret[0].hitbox[3] = Pos { row: 2, column: 0 };
    ret[1].hitbox[3] = Pos { row: 2, column: 2 };
    ret[2].hitbox[3] = Pos { row: 0, column: 2 };
    ret[3].hitbox[3] = Pos { row: 0, column: 0 };

    ret
}

const J_ROTATION: [Rotation; 4] = gen_j_rotation();

const fn gen_t_rotation() -> [Rotation; 4] {
    let mut ret = LINE_THREE_ROTATION;

    ret[0].hitbox[3] = Pos { row: 2, column: 1 };
    ret[1].hitbox[3] = Pos { row: 1, column: 2 };
    ret[2].hitbox[3] = Pos { row: 0, column: 1 };
    ret[3].hitbox[3] = Pos { row: 1, column: 0 };

    ret
}

const T_ROTATION: [Rotation; 4] = gen_t_rotation();

const fn gen_s_rotation() -> [Rotation; 4] {
    let mut ret = WALLKICK_TEMPLATE;

    let mut i = 0;
    while i < 2 {
        let mut j = 0;
        while j < 2 {
            ret[j * 2].hitbox[i] = Pos {
                row: (2 - j) as i8,
                column: (i + 1) as i8,
            };
            ret[j * 2].hitbox[i + 2] = Pos {
                row: (1 - j) as i8,
                column: i as i8,
            };

            ret[(j * 2) + 1].hitbox[i] = Pos {
                row: (i + 1) as i8,
                column: (1 - j) as i8,
            };
            ret[(j * 2) + 1].hitbox[i + 2] = Pos {
                row: i as i8,
                column: (2 - j) as i8,
            };
            j += 1;
        }

        i += 1;
    }

    ret
}

const S_ROTATION: [Rotation; 4] = gen_s_rotation();

const fn gen_z_rotation() -> [Rotation; 4] {
    let mut ret = WALLKICK_TEMPLATE;

    let mut i = 0;
    while i < 2 {
        let mut j = 0;
        while j < 2 {
            ret[j * 2].hitbox[i] = Pos {
                row: (2 - j) as i8,
                column: i as i8,
            };
            ret[j * 2].hitbox[i + 2] = Pos {
                row: (1 - j) as i8,
                column: (i + 1) as i8,
            };

            ret[(j * 2) + 1].hitbox[i] = Pos {
                row: i as i8,
                column: (1 - j) as i8,
            };
            ret[(j * 2) + 1].hitbox[i + 2] = Pos {
                row: (i + 1) as i8,
                column: (2 - j) as i8,
            };
            j += 1;
        }

        i += 1;
    }

    ret
}

const Z_ROTATION: [Rotation; 4] = gen_z_rotation();

pub(crate) static PIECE_DATA: [PieceData; 7] = [
    PieceData {
        rotations: I_ROTATION,
    },
    PieceData {
        rotations: O_ROTATION,
    },
    PieceData {
        rotations: L_ROTATION,
    },
    PieceData {
        rotations: J_ROTATION,
    },
    PieceData {
        rotations: T_ROTATION,
    },
    PieceData {
        rotations: S_ROTATION,
    },
    PieceData {
        rotations: Z_ROTATION,
    },
];
