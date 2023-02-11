use crate::position0x88::{Position, piece_colour, movegen::PIECE_TYPES_COUNT};

use super::{piece_type, EMPTY};

/// Piece values in centipawns.
pub const PIECE_VALUES: [u32; PIECE_TYPES_COUNT] = [0, 100, 500, 300, 325, 900, 2_000_000];

pub fn evaluate(position: &Position) -> i32 {

    let mut result: i32 = 0;

    // Evaluate material.
    for piece_square in 0..=0x77 {
        if piece_square & 0x88 != 0 {
            continue;
        }
        let piece = position.squares[piece_square];
        let piece_type = piece_type(piece);
        if piece_type == EMPTY {
            continue;
        }
        
        let colour = piece_colour(piece).unwrap();
        if colour == position.side_to_move {
            result += PIECE_VALUES[piece_type as usize] as i32;
        } else {
            result -= PIECE_VALUES[piece_type as usize] as i32;
        }

    }

    result
}