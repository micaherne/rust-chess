use crate::position0x88::{Position, piece_colour, movegen::PIECE_TYPES_COUNT};

use super::{piece_type, EMPTY, movegen::{side_to_move_in_check, can_evade_check}, SquareIndex, file, rank, WHITE, KING, square_iter};

pub type Score = i32;

pub type PieceSquareTable = [Score; 64];

pub const CHECKMATE_SCORE_MAX: Score = 20000;

const MIDDLEGAME: usize = 0;
const ENDGAME: usize = 1;

// TODO: This should be more sensible. It's basically just a fudge to use piece square tables
// from https://www.chessprogramming.org/Simplified_Evaluation_Function without having to 
// reformat them.

const PIECE_SQUARE_TABLE: [PieceSquareTable;6] = [
    [0; 64],
    [
        0,  0,  0,  0,  0,  0,  0,  0,
        50, 50, 50, 50, 50, 50, 50, 50,
        10, 10, 20, 30, 30, 20, 10, 10,
        5,  5, 10, 25, 25, 10,  5,  5,
        0,  0,  0, 20, 20,  0,  0,  0,
        5, -5,-10,  0,  0,-10, -5,  5,
        5, 10, 10,-20,-20, 10, 10,  5,
        0,  0,  0,  0,  0,  0,  0,  0
    ],
    [
        0,  0,  0,  0,  0,  0,  0,  0,
        5, 10, 10, 10, 10, 10, 10,  5,
       -5,  0,  0,  0,  0,  0,  0, -5,
       -5,  0,  0,  0,  0,  0,  0, -5,
       -5,  0,  0,  0,  0,  0,  0, -5,
       -5,  0,  0,  0,  0,  0,  0, -5,
       -5,  0,  0,  0,  0,  0,  0, -5,
        0,  0,  0,  5,  5,  0,  0,  0
    ],
    [
        -50,-40,-30,-30,-30,-30,-40,-50,
        -40,-20,  0,  0,  0,  0,-20,-40,
        -30,  0, 10, 15, 15, 10,  0,-30,
        -30,  5, 15, 20, 20, 15,  5,-30,
        -30,  0, 15, 20, 20, 15,  0,-30,
        -30,  5, 10, 15, 15, 10,  5,-30,
        -40,-20,  0,  5,  5,  0,-20,-40,
        -50,-40,-30,-30,-30,-30,-40,-50,
    ],
    [
        -20,-10,-10,-10,-10,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5, 10, 10,  5,  0,-10,
        -10,  5,  5, 10, 10,  5,  5,-10,
        -10,  0, 10, 10, 10, 10,  0,-10,
        -10, 10, 10, 10, 10, 10, 10,-10,
        -10,  5,  0,  0,  0,  0,  5,-10,
        -20,-10,-10,-10,-10,-10,-10,-20,
    ],
    [
        -20,-10,-10, -5, -5,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5,  5,  5,  5,  0,-10,
        -5,  0,  5,  5,  5,  5,  0, -5,
        0,  0,  5,  5,  5,  5,  0, -5,
        -10,  5,  5,  5,  5,  5,  0,-10,
        -10,  0,  5,  0,  0,  0,  0,-10,
        -20,-10,-10, -5, -5,-10,-10,-20
    ]
];

const KING_PIECE_SQUARE_TABLE: [PieceSquareTable; 2] = [
    [
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -20,-30,-30,-40,-40,-30,-30,-20,
        -10,-20,-20,-20,-20,-20,-20,-10,
        20, 20,  0,  0,  0,  0, 20, 20,
        20, 30, 10,  0,  0, 10, 30, 20
    ],
    [
        -50,-40,-30,-20,-20,-30,-40,-50,
        -30,-20,-10,  0,  0,-10,-20,-30,
        -30,-10, 20, 30, 30, 20,-10,-30,
        -30,-10, 30, 40, 40, 30,-10,-30,
        -30,-10, 30, 40, 40, 30,-10,-30,
        -30,-10, 20, 30, 30, 20,-10,-30,
        -30,-30,  0,  0,  0,  0,-30,-30,
        -50,-30,-30,-30,-30,-30,-30,-50
    ]
];

fn piece_square_index(index0x88: SquareIndex) -> SquareIndex {
    ((7 - rank(index0x88)) * 8 + file(index0x88)) as SquareIndex
}

/// The piece square table index reversed, for non-symmetrical tables.
fn piece_square_index_rev(index0x88: SquareIndex) -> SquareIndex {
    (rank(index0x88) * 8 + file(index0x88)) as SquareIndex
}

/// Piece values in centipawns.
pub const PIECE_VALUES: [Score; PIECE_TYPES_COUNT] = [0, 100, 500, 300, 325, 900, 2_000_000];

pub fn evaluate(position: &Position) -> Score {

    let mut result: Score = 0;

    if side_to_move_in_check(position) {
        if !can_evade_check(position) {
            return -CHECKMATE_SCORE_MAX;
        }
    }

    // Evaluate material.
    for piece_square in square_iter() {

        let piece = position.squares[piece_square];
        let piece_type = piece_type(piece);
        if piece_type == EMPTY {
            continue;
        }

        
        let colour = piece_colour(piece).unwrap();
        if colour == position.side_to_move {
            result += PIECE_VALUES[piece_type as usize] as Score;
        } else {
            result -= PIECE_VALUES[piece_type as usize] as Score;
        }

        // Reverse the index if it's black (for pawns and king).
        let piece_square_table_index = if colour == WHITE {
            piece_square_index(piece_square)
        } else {
            piece_square_index_rev(piece_square)
        };

        let table: PieceSquareTable;
        if piece_type == KING {
            // TODO: This needs to take account of the game phase - currently just using the middlegame one.
            table = KING_PIECE_SQUARE_TABLE[MIDDLEGAME];
        } else {
            table = PIECE_SQUARE_TABLE[piece_type as usize];
        }

        if colour == position.side_to_move {
            result += table[piece_square_table_index];
        } else {
            result -= table[piece_square_table_index];
        }

    }

    result
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_piece_square_index() {
        assert_eq!(56, piece_square_index(0x00));
        assert_eq!(57, piece_square_index(0x01));
        assert_eq!(0, piece_square_index(0x70));
        assert_eq!(7, piece_square_index(0x77));
    }

    #[test]
    fn test_piece_square_index_rev() {
        assert_eq!(56, piece_square_index_rev(0x70));
        assert_eq!(57, piece_square_index_rev(0x71));
        assert_eq!(0, piece_square_index_rev(0x00));
        assert_eq!(7, piece_square_index_rev(0x07));
    }
}