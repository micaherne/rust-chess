use std::fmt::Display;

use crate::{bitboards::PIECE_ATTACK_SQUARES, position::PIECE_COUNT};

use super::{
    movegen_bb::MoveGenerator,
    moves::{MakeMove, Move},
    Position64,
};

pub type Score = i32;

pub const CHECKMATE_SCORE_MAX: Score = 30000;

pub trait Evaluate {
    fn evaluate(&self) -> i32;
}

pub trait Quiesce {
    fn quiesce(&mut self, alpha: Score, beta: Score) -> Score;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PieceSquareTable([i32; 64]);

const PIECE_TABLES: [PieceSquareTable; PIECE_COUNT] = generate_tables();

const fn generate_tables() -> [PieceSquareTable; PIECE_COUNT] {
    let mut result = [PieceSquareTable([0; 64]); PIECE_COUNT];

    let mut piece = 1;
    while piece < PIECE_COUNT {
        let mut tables = [0; 64];
        let mut square = 0;
        let mut min = i32::MAX;
        let mut max = i32::MIN;
        while square < 64 {
            let val = PIECE_ATTACK_SQUARES[piece][square].count_ones() as i32;
            tables[square] = val;
            if val < min {
                min = val;
            }
            if val > max {
                max = val;
            }
            square += 1;
        }
        square = 0;
        while square < 64 {
            tables[square] = (tables[square] - min) - (max - min) / 2;
            square += 1;
        }
        result[piece] = PieceSquareTable(tables);
        piece += 1;
    }
    result
}

impl Evaluate for Position64 {
    fn evaluate(&self) -> Score {
        let mut score = 0;

        if self.is_check() {
            if !self.can_evade_check() {
                return -CHECKMATE_SCORE_MAX;
            }
        }

        /* Comedy piece square implementation.
        let mut square_score = 0;
        for piece in 1..PIECE_COUNT {
            if piece == PieceType::King as usize {
                continue;
            }
            let piece_bb = self.bb_pieces[piece];
            for piece_colour in [Colour::White, Colour::Black] {
                let mut bb = piece_bb & self.bb_colours[piece_colour as usize];
                iterate_squares!(bb -> square {
                    if piece_colour == self.side_to_move {
                        square_score += PIECE_TABLES[piece].0[square as usize];
                    } else {
                        square_score -= PIECE_TABLES[piece].0[square as usize];
                    }
                });
            }
        }

        score += square_score;*/

        score += self.material[self.side_to_move as usize]
            - self.material[self.side_to_move.opposite() as usize];

        score
    }
}

impl Quiesce for Position64 {
    fn quiesce(&mut self, alpha: Score, beta: Score) -> Score {
        let mut alpha_local = alpha;
        let stand_pat = self.evaluate();

        if stand_pat >= beta {
            return beta;
        }

        if stand_pat >= alpha_local {
            alpha_local = stand_pat;
        }

        let mut move_gen = MoveGenerator::default();
        move_gen.init(self);

        let moves: Vec<Move>;
        if move_gen.is_check() {
            moves = move_gen.generate_check_evasions();
        } else {
            // TODO: This should generate all useful captures, not just pawn ones.
            moves = move_gen.generate_pawn_captures();
        }

        for cap in moves {
            let undo = self.make_move(cap);
            let score = -self.quiesce(-beta, -alpha_local);
            self.undo_move(undo);
            if score >= beta {
                return beta;
            }
            if score > alpha_local {
                alpha_local = score;
            }
        }

        alpha_local
    }
}

impl Position64 {
    pub fn is_check(&self) -> bool {
        let mut move_gen = MoveGenerator::default();
        move_gen.init(self);
        move_gen.is_check()
    }

    pub fn can_evade_check(&self) -> bool {
        let mut move_gen = MoveGenerator::default();
        move_gen.init(self);
        let evasions = move_gen.generate_check_evasions();
        evasions.len() != 0
    }
}

impl Display for PieceSquareTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in (0..8).rev() {
            for j in 0..8 {
                write!(f, "{:3}", self.0[i * 8 + j])?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {

    use crate::position64::PieceType;

    use super::*;

    #[test]
    fn test_queen_table() {
        let p = generate_tables();
        println!("{}", p[PieceType::Queen as usize]);
    }
}
