use crate::{
    position::Evaluate,
    position0x88::{movegen_simple::PIECE_TYPES_COUNT, piece_colour, Position0x88},
};

use super::{
    file,
    movegen_simple::{can_evade_check, side_to_move_in_check},
    piece_type, rank, square_iter, SquareIndex, BISHOP, EMPTY, KING, KNIGHT, PAWN, QUEEN, ROOK,
    WHITE,
};

pub type Score = i32;

pub type PieceSquareTable = [Score; 64];

impl Evaluate<Score> for Position0x88 {
    fn evaluate(&self) -> Score {
        evaluate(&self)
    }
}

pub const CHECKMATE_SCORE_MAX: Score = 20000;

const MIDDLEGAME: usize = 0;
const ENDGAME: usize = 1;

const PHASE: [i32; PIECE_TYPES_COUNT] = [0, 0, 1, 1, 2, 4, 0];
const TOTAL_PHASE: i32 = PHASE[PAWN as usize] * 16
    + PHASE[KNIGHT as usize] * 4
    + PHASE[BISHOP as usize] * 4
    + PHASE[ROOK as usize] * 4
    + PHASE[QUEEN as usize] * 2;

// TODO: This should be more sensible. It's basically just a fudge to use piece square tables
// from https://www.chessprogramming.org/Simplified_Evaluation_Function without having to
// reformat them.

const PIECE_SQUARE_TABLE: [PieceSquareTable; 6] = [
    [0; 64],
    [
        0, 0, 0, 0, 0, 0, 0, 0, 50, 50, 50, 50, 50, 50, 50, 50, 10, 10, 20, 30, 30, 20, 10, 10, 5,
        5, 10, 25, 25, 10, 5, 5, 0, 0, 0, 20, 20, 0, 0, 0, 5, -5, -10, 0, 0, -10, -5, 5, 5, 10, 10,
        -20, -20, 10, 10, 5, 0, 0, 0, 0, 0, 0, 0, 0,
    ],
    [
        0, 0, 0, 0, 0, 0, 0, 0, 5, 10, 10, 10, 10, 10, 10, 5, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0,
        0, 0, 0, 0, -5, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0, 0, 0, 0, 0, -5, -5, 0, 0, 0, 0, 0, 0,
        -5, 0, 0, 0, 5, 5, 0, 0, 0,
    ],
    [
        -50, -40, -30, -30, -30, -30, -40, -50, -40, -20, 0, 0, 0, 0, -20, -40, -30, 0, 10, 15, 15,
        10, 0, -30, -30, 5, 15, 20, 20, 15, 5, -30, -30, 0, 15, 20, 20, 15, 0, -30, -30, 5, 10, 15,
        15, 10, 5, -30, -40, -20, 0, 5, 5, 0, -20, -40, -50, -40, -30, -30, -30, -30, -40, -50,
    ],
    [
        -20, -10, -10, -10, -10, -10, -10, -20, -10, 0, 0, 0, 0, 0, 0, -10, -10, 0, 5, 10, 10, 5,
        0, -10, -10, 5, 5, 10, 10, 5, 5, -10, -10, 0, 10, 10, 10, 10, 0, -10, -10, 10, 10, 10, 10,
        10, 10, -10, -10, 5, 0, 0, 0, 0, 5, -10, -20, -10, -10, -10, -10, -10, -10, -20,
    ],
    [
        -20, -10, -10, -5, -5, -10, -10, -20, -10, 0, 0, 0, 0, 0, 0, -10, -10, 0, 5, 5, 5, 5, 0,
        -10, -5, 0, 5, 5, 5, 5, 0, -5, 0, 0, 5, 5, 5, 5, 0, -5, -10, 5, 5, 5, 5, 5, 0, -10, -10, 0,
        5, 0, 0, 0, 0, -10, -20, -10, -10, -5, -5, -10, -10, -20,
    ],
];

const KING_PIECE_SQUARE_TABLE: [PieceSquareTable; 2] = [
    [
        -30, -40, -40, -50, -50, -40, -40, -30, -30, -40, -40, -50, -50, -40, -40, -30, -30, -40,
        -40, -50, -50, -40, -40, -30, -30, -40, -40, -50, -50, -40, -40, -30, -20, -30, -30, -40,
        -40, -30, -30, -20, -10, -20, -20, -20, -20, -20, -20, -10, 20, 20, 0, 0, 0, 0, 20, 20, 20,
        30, 10, 0, 0, 10, 30, 20,
    ],
    [
        -50, -40, -30, -20, -20, -30, -40, -50, -30, -20, -10, 0, 0, -10, -20, -30, -30, -10, 20,
        30, 30, 20, -10, -30, -30, -10, 30, 40, 40, 30, -10, -30, -30, -10, 30, 40, 40, 30, -10,
        -30, -30, -10, 20, 30, 30, 20, -10, -30, -30, -30, 0, 0, 0, 0, -30, -30, -50, -30, -30,
        -30, -30, -30, -30, -50,
    ],
];

fn piece_square_index(index0x88: SquareIndex) -> SquareIndex {
    ((7 - rank(index0x88)) * 8 + file(index0x88)) as SquareIndex
}

/// The piece square table index reversed, for non-symmetrical tables.
fn piece_square_index_rev(index0x88: SquareIndex) -> SquareIndex {
    (rank(index0x88) * 8 + file(index0x88)) as SquareIndex
}

fn game_phase(position: &Position0x88) -> i32 {
    let mut phase = TOTAL_PHASE;
    for sq in square_iter() {
        let piece_type = piece_type(position.squares[sq]);
        phase -= PHASE[piece_type as usize];
    }
    (phase * 256 + (TOTAL_PHASE / 2)) / TOTAL_PHASE
}

/// Piece values in centipawns.
pub const PIECE_VALUES: [Score; PIECE_TYPES_COUNT] = [0, 100, 500, 300, 325, 900, 2_000_000];

pub fn evaluate(position: &Position0x88) -> Score {
    let mut middlegame_score: Score = 0;
    let mut endgame_score: Score = 0;

    let phase = game_phase(position);

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
            middlegame_score += PIECE_VALUES[piece_type as usize] as Score;
            endgame_score += PIECE_VALUES[piece_type as usize] as Score;
        } else {
            middlegame_score -= PIECE_VALUES[piece_type as usize] as Score;
            endgame_score -= PIECE_VALUES[piece_type as usize] as Score;
        }

        // Reverse the index if it's black (for pawns and king).
        let piece_square_table_index = if colour == WHITE {
            piece_square_index(piece_square)
        } else {
            piece_square_index_rev(piece_square)
        };

        let mtable: PieceSquareTable;
        let etable: PieceSquareTable;
        if piece_type == KING {
            mtable = KING_PIECE_SQUARE_TABLE[MIDDLEGAME];
            etable = KING_PIECE_SQUARE_TABLE[ENDGAME];
        } else {
            mtable = PIECE_SQUARE_TABLE[piece_type as usize];
            etable = PIECE_SQUARE_TABLE[piece_type as usize];
        }

        if colour == position.side_to_move {
            middlegame_score += mtable[piece_square_table_index];
            endgame_score += etable[piece_square_table_index];
        } else {
            middlegame_score -= mtable[piece_square_table_index];
            endgame_score -= etable[piece_square_table_index];
        }
    }

    // TODO: This is probably wrong as it assumes that both scores are positive (I think)
    // and won't really work when they're negative, or at least the order will be important.
    ((middlegame_score * (256 - phase)) + (endgame_score * phase)) / 256
}

#[cfg(test)]
mod test {
    use crate::fen::STARTPOS_FEN;

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

    #[test]
    fn test_game_phase() {
        let pos: Position0x88 = STARTPOS_FEN.into();
        assert_eq!(0, game_phase(&pos));
        let pos: Position0x88 = "8/8/5k2/8/K7/8/8/8 w - - 0 1".into();
        assert_eq!(256, game_phase(&pos));
    }
}
