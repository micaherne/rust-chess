pub const STARTPOS_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[derive(Debug)]
pub enum FenError {
    IncorrectNumberOfParts,
    InvalidCastlingInfo,
    InvalidDigit,
    InvalidLine,
    InvalidPiece,
    InvalidSquare,
    InvalidColourToMove,
}

impl std::fmt::Display for FenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid FEN")
    }
}

pub struct Fen {
    pub string: String,
    pub piece_placements: String,
    pub side_to_move: String,
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

        Ok(Self {
            string: value.to_string(),
            piece_placements: parts[0].to_string(),
            side_to_move: parts[1].to_string(),
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
