pub mod movegen;

use crate::position0x88::{index0x88to64, movegen::PIECE_TYPES_COUNT, SquareIndex, BLACK, WHITE, PieceType, BISHOP, QUEEN, ROOK};

pub type Bitboard = u64;
pub type SquareIndex64 = u8;
pub type Direction = i8;

/// Index for a file, rank, diagonal or antidiagonal.
pub type LineIndex = u8;

pub type EightBitboards = [Bitboard; 8];
pub type SixteenBitboards = [Bitboard; 16];
pub type SixtyFourBitboards = [Bitboard; 64];

pub const DIR_KNIGHT: [Direction; 8] = [-17, -15, -10, -6, 6, 10, 15, 17];

/// Directions. Linear then diagonal.
pub const DIR_ALL_SLIDERS: [i8; 8] = [-1, 1, 8, -8, 7, -7, 9, -9];
pub const fn dir_index(dir: i8) -> usize {
    match dir {
        -1 => 0,
        1 => 1,
        8 => 2,
        -8 => 3,
        7 => 4,
        -7 => 5,
        9 => 6,
        -9 => 7,
        _ => panic!("Invalid direction")
    }
}

pub const fn is_linear(dir: Direction) -> bool {
    dir.abs() == 1 || dir.abs() == 8
}

pub const fn is_diagonal(dir: Direction) -> bool {
    dir.abs() == 7 || dir.abs() == 9
}

pub const fn is_slider(piece_type: PieceType) -> bool {
    piece_type == BISHOP || piece_type == ROOK || piece_type == QUEEN
}

pub const fn slides_in_dir(piece_type: PieceType, dir: i8) -> bool {
    debug_assert!(is_slider(piece_type));

    let dir_index = dir_index(dir);
    match piece_type {
        BISHOP => dir_index > 3,
        ROOK => dir_index < 4,
        QUEEN => true,
        _ => false
    }
}

pub const RANK_MASK: EightBitboards = init_rank_masks();
pub const FILE_MASK: EightBitboards = init_file_masks();
pub const DIAGONAL_MASK: SixteenBitboards = init_diagonal_masks();
pub const ANTIDIAGONAL_MASK: SixteenBitboards = init_antidiagonal_masks();

/// Bitboards of squares in a given direction from a square.
pub const SLIDER_DIRECTION_SQUARE: [SixtyFourBitboards; DIR_ALL_SLIDERS.len()] =
    init_slider_dir_squares();

pub const DIRECTION_FROM_TO: [[i8; 64]; 64] = init_dir_and_between().0;
pub const BETWEEN: [SixtyFourBitboards; 64] = init_dir_and_between().1;

pub const PAWN_ATTACK_SQUARES: [SixtyFourBitboards; 2] = init_pawn_attacks();
pub const ROOK_ATTACK_SQUARES: SixtyFourBitboards = init_rook_attacks();
pub const KNIGHT_ATTACK_SQUARES: SixtyFourBitboards = init_knight_attacks();
pub const BISHOP_ATTACK_SQUARES: SixtyFourBitboards = init_bishop_attacks();
pub const QUEEN_ATTACK_SQUARES: SixtyFourBitboards = init_queen_attacks();
pub const KING_ATTACK_SQUARES: SixtyFourBitboards = init_king_attacks();

pub const PIECE_ATTACK_SQUARES: [SixtyFourBitboards; PIECE_TYPES_COUNT] = [
    [0; 64],
    [0; 64],
    ROOK_ATTACK_SQUARES,
    KNIGHT_ATTACK_SQUARES,
    BISHOP_ATTACK_SQUARES,
    QUEEN_ATTACK_SQUARES,
    KING_ATTACK_SQUARES,
];

#[inline]
pub fn squares_from_bitboard(bitboard: Bitboard) -> Vec<SquareIndex64> {
    let mut bb = bitboard;
    let mut result: Vec<SquareIndex64> = vec![];
    while bb != 0 {
        result.push(bitboard.trailing_zeros() as SquareIndex64);
        bb &= bb - 1;
    }
    result
}

/// Get the highest set bit.
/// Note that this will panic if the bitboard is zero so we must check that before calling.
#[inline]
pub fn highest_set_bit(bitboard: Bitboard) -> SquareIndex64 {
    debug_assert!(bitboard != 0);

    bitboard.ilog2() as SquareIndex64
}

/// Get the lowest set bit.
/// Note that this will return an invalid value (64) if the bitboard is zero so we must check that before calling.
/// (We don't check for zero in this function for speed; it's usually known in calling code if it is zero.)
#[inline]
pub fn lowest_set_bit(bitboard: Bitboard) -> SquareIndex64 {
    debug_assert!(bitboard != 0);

    bitboard.trailing_zeros() as SquareIndex64
}

/// Take each bit of a bitboard and return a vector of indices for them.
macro_rules! unwrap_bitboard {
    ($bb:ident) => {{
        let mut x = $bb as Bitboard;
        let mut result: Vec<SquareIndex64> = vec![];
        while x != 0 {
            result.push(x.trailing_zeros() as SquareIndex64);
            x &= x - 1;
        }
        result
    }};

    ($bb:ident:reverse) => {{
        let mut b = $bb as Bitboard;
        let mut result: Vec<SquareIndex64> = vec![];
        while b != 0 {
            let x = 63 - b.leading_zeros();
            result.push(x as SquareIndex64);
            b &= (1 << x) - 1;
        }
        result
    }};

    ($bb:expr) => {{
        let b = $bb as Bitboard;
        unwrap_bitboard!(b)
    }};
}
pub(crate) use unwrap_bitboard;

const fn init_rank_masks() -> EightBitboards {
    let mut result = [0; 8];
    let mut i = 0;
    while i < 8 {
        result[i] = 0b11111111 << (8 * i);
        i += 1;
    }
    result
}

const fn init_file_masks() -> EightBitboards {
    let mut result = [0; 8];
    let mut i = 0;
    while i < 8 {
        result[i] = 0x101010101010101 << i;
        i += 1;
    }
    result
}

const fn init_diagonal_masks() -> SixteenBitboards {
    let mut diagonal_masks = [0; 16];
    let mut i = 0;
    while i < 8 {
        diagonal_masks[7 - i] = SLIDER_DIRECTION_SQUARE[6][i] | 1 << i;
        diagonal_masks[i + 7] = SLIDER_DIRECTION_SQUARE[6][8 * i] | 1 << (8 * i);

        i += 1;
    }
    diagonal_masks
}

const fn init_antidiagonal_masks() -> SixteenBitboards {
    let mut antidiagonal_masks = [0; 16];
    let mut i = 0;
    while i < 8 {
        antidiagonal_masks[i] = SLIDER_DIRECTION_SQUARE[4][i] | 1 << i;
        antidiagonal_masks[i + 7] = SLIDER_DIRECTION_SQUARE[4][7 + 8 * i] | 1 << (7 + 8 * i);
        i += 1;
    }
    antidiagonal_masks
}

const fn init_slider_dir_squares() -> [SixtyFourBitboards; DIR_ALL_SLIDERS.len()] {
    let mut result: [SixtyFourBitboards; DIR_ALL_SLIDERS.len()] = [[0; 64]; DIR_ALL_SLIDERS.len()];
    let mut sq = 0;
    while sq < 64 {
        let mut dir_index = 0;
        while dir_index < DIR_ALL_SLIDERS.len() {
            let mut to = sq as i8;
            let mut i2 = 0;
            while i2 < 8 {
                let from_rank = rank(to as u8);
                let from_file = file(to as u8);
                to += DIR_ALL_SLIDERS[dir_index];
                if to < 0 || to > 63 {
                    break;
                }
                if rank(to as SquareIndex64).abs_diff(from_rank) > 1
                    || file(to as SquareIndex64).abs_diff(from_file) > 1
                {
                    break;
                }
                result[dir_index][sq as usize] |= 1 << to;
                i2 += 1;
            }
            dir_index += 1;
        }
        sq += 1;
    }

    result
}

const fn init_dir_and_between() -> ([[i8; 64]; 64], [SixtyFourBitboards; 64]) {
    let mut direction_from_to = [[0; 64]; 64];
    let mut between = [[0; 64]; 64];
    let mut from = 0;
    while from < 64 {
        let mut i = 0;
        while i < DIR_ALL_SLIDERS.len() {
            let dir = DIR_ALL_SLIDERS[i];
            let mut squares = SLIDER_DIRECTION_SQUARE[i][from];
            while squares != 0 {
                let to = squares.trailing_zeros();
                direction_from_to[from][to as usize] = dir;
                let previous = to as i8 - dir;

                debug_assert!(previous >= 0);

                between[from][to as usize] = SLIDER_DIRECTION_SQUARE[i][from]
                    ^ SLIDER_DIRECTION_SQUARE[i][previous as usize];
                squares &= squares - 1;
            }
            i += 1;
        }
        from += 1;
    }
    (direction_from_to, between)
}

const fn init_pawn_attacks() -> [SixtyFourBitboards; 2] {
    let mut pawn_attack_squares = [[0; 64]; 2];

    let mut sq = 8; // Makes no sense on first rank.

    while sq < 56 {
        if file(sq) != 0 {
            pawn_attack_squares[WHITE as usize][sq as usize] |= 1 << (sq + 7);
            pawn_attack_squares[BLACK as usize][sq as usize] |= 1 << (sq - 9);
        }
        if file(sq) != 7 {
            pawn_attack_squares[WHITE as usize][sq as usize] |= 1 << (sq + 9);
            pawn_attack_squares[BLACK as usize][sq as usize] |= 1 << (sq - 7);
        }
        sq += 1;
    }

    pawn_attack_squares
}

const fn init_rook_attacks() -> SixtyFourBitboards {
    let mut rook_attacks = [0; 64];
    let mut i = 0;
    while i < 64 {
        rook_attacks[i as usize] |= RANK_MASK[rank(i) as usize];
        rook_attacks[i as usize] |= FILE_MASK[file(i) as usize];
        rook_attacks[i as usize] &= !(1 << i);
        i += 1;
    }
    rook_attacks
}

const fn init_knight_attacks() -> SixtyFourBitboards {
    let mut knight_attacks = [0; 64];

    let mut i = 0;
    while i < 64 {
        let from_rank = rank(i);
        let from_file = file(i);
        let mut i2 = 0;
        while i2 < DIR_KNIGHT.len() {
            let d = DIR_KNIGHT[i2];
            let to_square = i as i8 + d;
            if to_square < 0 || to_square > 63 {
                i2 += 1;
                continue;
            }
            let to_rank = rank(to_square as SquareIndex64);
            let to_file = file(to_square as SquareIndex64);

            if from_rank.abs_diff(to_rank) > 2 || from_file.abs_diff(to_file) > 2 {
                i2 += 1;
                continue;
            }
            knight_attacks[i as usize] |= 1 << to_square;
            i2 += 1;
        }
        i += 1;
    }

    knight_attacks
}

const fn init_bishop_attacks() -> SixtyFourBitboards {
    let mut attack_squares = [0; 64];
    let mut sq = 0;
    while sq < 64 {
        // Diagonal directions start at 4.
        let mut dir = 4;
        while dir < DIR_ALL_SLIDERS.len() {
            attack_squares[sq] |= SLIDER_DIRECTION_SQUARE[dir][sq];
            dir += 1;
        }
        sq += 1;
    }
    attack_squares
}

const fn init_queen_attacks() -> SixtyFourBitboards {
    let mut queen_attacks = [0; 64];
    let mut sq = 0;
    while sq < 64 {
        queen_attacks[sq] |= ROOK_ATTACK_SQUARES[sq];
        queen_attacks[sq] |= BISHOP_ATTACK_SQUARES[sq];
        sq += 1;
    }
    queen_attacks
}

const fn init_king_attacks() -> SixtyFourBitboards {
    let mut king_attacks = [0; 64];
    let mut sq = 0;
    while sq < 64 {
        let from_rank = rank(sq as u8);
        let from_file = file(sq as u8);
        let mut i = 0;
        while i < DIR_ALL_SLIDERS.len() {
            let dir = DIR_ALL_SLIDERS[i];
            let to = sq as i8 + dir;
            if to < 0 || to > 63 {
                break;
            }
            if rank(to as SquareIndex64).abs_diff(from_rank) > 1
                || file(to as SquareIndex64).abs_diff(from_file) > 1
            {
                i += 1;
                continue;
            }
            king_attacks[sq] |= 1 << to;
            i += 1;
        }
        sq += 1;
    }
    king_attacks
}

#[inline]
pub const fn square_mask0x88(square: SquareIndex) -> Bitboard {
    1 << index0x88to64(square)
}

pub const fn square_mask64(square: SquareIndex64) -> Bitboard {
    1 << square
}

#[inline]
pub const fn rank(square: SquareIndex64) -> LineIndex {
    square >> 3
}

#[inline]
pub const fn file(square: SquareIndex64) -> LineIndex {
    square & 7
}

#[inline]
pub const fn diagonal(square: SquareIndex64) -> LineIndex {
    7 + rank(square) - file(square)
}

#[inline]
pub const fn antidiagonal(square: SquareIndex64) -> LineIndex {
    rank(square) + file(square)
}

trait BitboardDisplay {
    fn bitboard_to_string(&self) -> String;
}

impl BitboardDisplay for Bitboard {
    fn bitboard_to_string(&self) -> String {
        let mut result = String::new();
        let mask: Bitboard = 0xFF;
        for i in (0..8).rev() {
            let shifted = self >> (8 * i);
            let line: String = format!("{:08b}", shifted & mask)
                .replace("0", ".")
                .chars()
                .rev()
                .collect();
            result.push_str(&line);
            result.push('\n');
        }
        result
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_bitboard_constants() {
        assert_eq!(9, DIRECTION_FROM_TO[17][62]);
        assert_eq!(0, DIRECTION_FROM_TO[17][61]);

        assert_eq!(0x2010080402010000, DIAGONAL_MASK[9]);
        assert_eq!(0x8040201008040201, DIAGONAL_MASK[7]);
        assert_eq!(0x102040810, ANTIDIAGONAL_MASK[4]);
        assert_eq!(0x1400000000, PAWN_ATTACK_SQUARES[WHITE as usize][27]);
        assert_eq!(0x2, PAWN_ATTACK_SQUARES[BLACK as usize][8]);
    }

    #[test]
    fn test_unwrap_bitboard() {
        assert_eq!(
            vec![19, 44, 49],
            unwrap_bitboard!(0x2100000080000 as Bitboard)
        );
        let x1 = 0x2100000080000 as Bitboard;
        assert_eq!(
            vec![49, 44, 19],
            unwrap_bitboard!(x1:reverse)
        );
    }

    #[test]
    fn test_highest_set_bit() {
        assert_eq!(0, highest_set_bit(1));
        assert_eq!(63, highest_set_bit(Bitboard::MAX));
    }

    #[test]
    fn test_lowest_set_bit() {
        assert_eq!(0, lowest_set_bit(1));
        assert_eq!(0, lowest_set_bit(Bitboard::MAX));
    }
}
