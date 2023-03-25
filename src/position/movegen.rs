use chess_uci::messages::LongAlgebraicNotationMove;

use super::{
    notation::{piece_type_to_char, square_index_to_str},
    Piece, SquareIndex,
};

pub trait GenerateMoves {
    fn generate_moves(&self) -> Vec<Move>;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Move {
    pub from_index: SquareIndex,
    pub to_index: SquareIndex,
    pub queening_piece: Option<Piece>,
}

impl ToString for Move {
    fn to_string(&self) -> String {
        let mut result = String::new();
        result.push_str(square_index_to_str(self.from_index).as_str());
        result.push_str(square_index_to_str(self.to_index).as_str());
        match self.queening_piece {
            Some(piece) => result.push(piece_type_to_char(piece).or(Some(' ')).unwrap()),
            None => (),
        }
        result
    }
}

impl From<&Move> for LongAlgebraicNotationMove {
    fn from(value: &Move) -> Self {
        let mut text = String::new();
        text.push_str(&square_index_to_str(value.from_index));
        text.push_str(&square_index_to_str(value.to_index));
        if let Some(q) = value.queening_piece {
            text.push(piece_type_to_char(q).unwrap_or(' '));
        }
        LongAlgebraicNotationMove { text }
    }
}
