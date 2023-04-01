use std::{fmt::Display, str::FromStr};

use crate::position::{Piece, SetPosition, SquareIndex};

pub const STARTPOS_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[derive(Debug)]
pub enum FenError {
    IncorrectNumberOfParts,
    InvalidCastlingInfo(String),
    InvalidDigit(String),
    InvalidLine,
    InvalidPiece(char),
    InvalidSquare(String),
    InvalidColourToMove(String),
}

impl std::fmt::Display for FenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid FEN: {}", self)
    }
}

pub struct Fen {
    pub string: String,
    pub piece_placements: String,
    pub side_to_move: char,
    pub castling: String,
    pub ep_square: String,
    pub halfmove: String,
    pub fullmove: String,
}

impl Fen {
    pub fn from_startpos() -> Self {
        let s: Fen = STARTPOS_FEN.try_into().unwrap();
        s.try_into().unwrap()
    }

    pub fn set_piece_placements<T: SetPosition<S, P>, S: SquareIndex, P: Piece>(
        &self,
        position: &mut T,
    ) -> Result<(), FenError> {
        if self.piece_placements.split('/').count() != 8 {
            return Err(FenError::IncorrectNumberOfParts);
        }

        let mut rank = 7;
        let mut file = 0;

        for c in self.piece_placements.chars() {
            if c.is_numeric() {
                let skip_num = c.to_digit(10);
                match skip_num {
                    Some(empty_count) => {
                        if empty_count > 8 {
                            return Err(FenError::InvalidDigit(c.to_string()));
                        }
                        for _ in 0..empty_count {
                            position.set_square_to_piece(
                                S::from_rank_and_file(rank, file),
                                P::from_algebraic_notation(' ')?,
                            );
                            file += 1;
                        }
                    }
                    None => return Err(FenError::InvalidDigit(c.to_string())),
                }
            } else if c == '/' {
                if file != 8 {
                    return Err(FenError::InvalidLine);
                }
                rank -= 1;
                file = 0;
            } else {
                position.set_square_to_piece(
                    S::from_rank_and_file(rank, file),
                    P::from_algebraic_notation(c)?,
                );
                file += 1;
            }
        }

        Ok(())
    }
}

impl PartialEq for Fen {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl Display for Fen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl FromStr for Fen {
    type Err = FenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let fen: Fen = s.try_into()?;
        Ok(fen)
    }
}

impl TryFrom<&str> for Fen {
    type Error = FenError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        // Check count first as it uses up the iterator.
        let cnt = value.split_ascii_whitespace().count();
        if cnt != 4 && cnt != 6 {
            return Err(FenError::IncorrectNumberOfParts);
        }

        let parts: Vec<&str> = value.split_ascii_whitespace().collect();

        if parts[1] != "w" && parts[1] != "b" {
            return Err(FenError::InvalidColourToMove(parts[1].to_string()));
        }

        let side_to_move = parts[1].chars().next().unwrap();

        Ok(Self {
            string: value.to_string(),
            piece_placements: parts[0].to_string(),
            side_to_move,
            castling: parts[2].to_string(),
            ep_square: parts[3].to_string(),
            halfmove: (*parts.get(4).unwrap_or(&"0")).to_string(),
            fullmove: (*parts.get(5).unwrap_or(&"1")).to_string(),
        })
    }
}

pub trait ConsumeFen {
    fn set_from_fen(&mut self, fen: Fen) -> Result<(), FenError>;

    fn set_startpos(&mut self) -> Result<(), FenError> {
        self.set_from_fen(Fen::from_startpos())
    }
}

// Serialising to fen should be done by implementing Into<Fen>.
