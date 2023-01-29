use regex::Regex;

pub const STARTPOS_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[derive(Debug)]
pub enum FenError {
    IncorrectNumberOfParts,
    InvalidCastlingInfo,
    InvalidDigit,
    InvalidLine,
    InvalidPiece,
    InvalidSquare,
    InvalidColourToMove
}

impl std::fmt::Display for FenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid FEN")
    }
}
