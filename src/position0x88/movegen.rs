use std::fmt::Display;

use chess_uci::messages::LongAlgebraicNotationMove;

use crate::{
    moves::Move,
    position::{Piece, SquareIndex},
};

use super::{notation::piece_type_to_char, PieceStandard, SquareIndex0x88};

impl<S: SquareIndex, P: Piece> Display for Move<S, P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.from_index.sq_to_algebraic_notation(),
            self.to_index.sq_to_algebraic_notation()
        )?;
        if let Some(q) = &self.queening_piece {
            write!(f, "{}", q.to_algebraic_notation())?;
        }
        Ok(())
    }
}

impl<S: SquareIndex, P: Piece> From<Move<S, P>> for LongAlgebraicNotationMove {
    fn from(value: Move<S, P>) -> Self {
        LongAlgebraicNotationMove {
            text: value.to_string(),
        }
    }
}

pub type Move0x88 = Move<SquareIndex0x88, PieceStandard>;

impl From<&Move0x88> for LongAlgebraicNotationMove {
    fn from(value: &Move0x88) -> Self {
        let mut text = String::new();
        text.push_str(value.from_index.sq_to_algebraic_notation().as_str());
        text.push_str(value.to_index.sq_to_algebraic_notation().as_str());
        if let Some(q) = value.queening_piece {
            text.push(piece_type_to_char(q).unwrap_or(' '));
        }
        LongAlgebraicNotationMove { text }
    }
}
