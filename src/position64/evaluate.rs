use super::{movegen_bb::MoveGenerator, Position64};

pub type Score = i32;

pub const CHECKMATE_SCORE_MAX: Score = 30000;

pub trait Evaluate {
    fn evaluate(&self) -> i32;
}

impl Evaluate for Position64 {
    fn evaluate(&self) -> Score {
        let mut score = 0;

        if self.is_check() {
            if !self.can_evade_check() {
                return -CHECKMATE_SCORE_MAX;
            }
        }

        score += self.material[self.side_to_move as usize]
            - self.material[self.side_to_move.opposite() as usize];

        score
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
