pub mod evaluate;
pub mod movegen_bb;
pub mod moves;
use std::{
    fmt::{Debug, Display, Error},
    str::FromStr,
};

use crate::{
    bitboards::{file, Bitboard, BitboardOps, SquareIndex64},
    fen::{Fen, FenError},
    position::{Castling, CastlingRights, Piece, SetPosition, SquareIndex, PIECE_COUNT},
    transposition::Hashable,
    zobrist::{ZobristNumber, ZOBRIST_NUMBERS},
};

use super::position::HasCastlingRights;

/// The colour of a piece.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Colour {
    White,
    Black,
    #[default]
    None,
}

impl Colour {
    pub fn opposite(&self) -> Self {
        match self {
            Colour::White => Colour::Black,
            Colour::Black => Colour::White,
            Colour::None => Colour::None,
        }
    }
    pub fn to_algebraic_notation(&self) -> String {
        match self {
            Colour::White => "w".to_string(),
            Colour::Black => "b".to_string(),
            Colour::None => "-".to_string(),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum PieceType {
    #[default]
    Empty,
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

impl From<usize> for PieceType {
    fn from(index: usize) -> Self {
        match index {
            1 => PieceType::Pawn,
            2 => PieceType::Rook,
            3 => PieceType::Knight,
            4 => PieceType::Bishop,
            5 => PieceType::Queen,
            6 => PieceType::King,
            _ => PieceType::Empty,
        }
    }
}

impl AlgebraicNotation for PieceType {
    fn to_algebraic_notation(&self) -> String {
        match self {
            PieceType::Pawn => "p".to_string(),
            PieceType::Rook => "r".to_string(),
            PieceType::Knight => "n".to_string(),
            PieceType::Bishop => "b".to_string(),
            PieceType::Queen => "q".to_string(),
            PieceType::King => "k".to_string(),
            PieceType::Empty => " ".to_string(),
        }
    }

    fn from_algebraic_notation(algebraic_notation: &str) -> Result<Self, FenError>
    where
        Self: Sized,
    {
        match algebraic_notation {
            "p" => Ok(PieceType::Pawn),
            "r" => Ok(PieceType::Rook),
            "n" => Ok(PieceType::Knight),
            "b" => Ok(PieceType::Bishop),
            "q" => Ok(PieceType::Queen),
            "k" => Ok(PieceType::King),
            " " => Ok(PieceType::Empty),
            _ => Err(FenError::InvalidPiece(
                algebraic_notation.chars().nth(0).unwrap(),
            )),
        }
    }
}

impl PieceType {
    pub fn evaluation_value(&self) -> i32 {
        match self {
            PieceType::Pawn => 100,
            PieceType::Knight => 320,
            PieceType::Bishop => 330,
            PieceType::Rook => 500,
            PieceType::Queen => 900,
            PieceType::King => 20000,
            PieceType::Empty => 0,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PieceWithColour {
    pub piece_type: PieceType,
    pub colour: Colour,
}

impl PieceWithColour {
    pub fn new(piece_type: PieceType, colour: Colour) -> Self {
        Self { piece_type, colour }
    }
    pub fn empty() -> Self {
        Self {
            piece_type: PieceType::Empty,
            colour: Colour::None,
        }
    }
}

impl Piece for PieceWithColour {
    fn to_algebraic_notation(&self) -> String {
        match self.colour {
            Colour::White => match self.piece_type {
                PieceType::Pawn => "P".to_string(),
                PieceType::Rook => "R".to_string(),
                PieceType::Knight => "N".to_string(),
                PieceType::Bishop => "B".to_string(),
                PieceType::Queen => "Q".to_string(),
                PieceType::King => "K".to_string(),
                PieceType::Empty => " ".to_string(),
            },
            Colour::Black => match self.piece_type {
                PieceType::Pawn => "p".to_string(),
                PieceType::Rook => "r".to_string(),
                PieceType::Knight => "n".to_string(),
                PieceType::Bishop => "b".to_string(),
                PieceType::Queen => "q".to_string(),
                PieceType::King => "k".to_string(),
                PieceType::Empty => " ".to_string(),
            },
            Colour::None => " ".to_string(),
        }
    }

    fn from_algebraic_notation(algebraic_notation: char) -> Result<Self, FenError>
    where
        Self: Sized,
    {
        let piece_type = match algebraic_notation {
            'P' => PieceType::Pawn,
            'R' => PieceType::Rook,
            'N' => PieceType::Knight,
            'B' => PieceType::Bishop,
            'Q' => PieceType::Queen,
            'K' => PieceType::King,
            'p' => PieceType::Pawn,
            'r' => PieceType::Rook,
            'n' => PieceType::Knight,
            'b' => PieceType::Bishop,
            'q' => PieceType::Queen,
            'k' => PieceType::King,
            ' ' => PieceType::Empty,
            _ => return Err(FenError::InvalidPiece(algebraic_notation)),
        };
        let colour = match algebraic_notation {
            'P' => Colour::White,
            'R' => Colour::White,
            'N' => Colour::White,
            'B' => Colour::White,
            'Q' => Colour::White,
            'K' => Colour::White,
            'p' => Colour::Black,
            'r' => Colour::Black,
            'n' => Colour::Black,
            'b' => Colour::Black,
            'q' => Colour::Black,
            'k' => Colour::Black,
            ' ' => Colour::None,
            _ => return Err(FenError::InvalidPiece(algebraic_notation)),
        };
        Ok(Self::new(piece_type, colour))
    }
}

pub trait AlgebraicNotation {
    fn to_algebraic_notation(&self) -> String;
    fn from_algebraic_notation(algebraic_notation: &str) -> Result<Self, FenError>
    where
        Self: Sized;
}

pub type SixtyFourPieces = [PieceWithColour; 64];

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct CastlingRightsBool([bool; 4]);

impl CastlingRights for CastlingRightsBool {}

#[derive(Clone, Copy)]
pub struct Position64 {
    squares: SixtyFourPieces,
    side_to_move: Colour,
    castling_rights: CastlingRightsBool,
    ep_square: Bitboard,
    halfmove_clock: u32,
    fullmove_number: u32,
    hash_key: ZobristNumber,

    // bitboards
    pub bb_pieces: [Bitboard; PIECE_COUNT],
    pub bb_colours: [Bitboard; 3],

    // evaluation related (index 2 is meaningless, it just prevents branching)
    pub material: [i32; 3],
}

impl Default for Position64 {
    fn default() -> Self {
        Self {
            squares: [PieceWithColour::empty(); 64],
            side_to_move: Default::default(),
            castling_rights: Default::default(),
            ep_square: Default::default(),
            halfmove_clock: Default::default(),
            fullmove_number: Default::default(),
            hash_key: Default::default(),
            bb_pieces: Default::default(),
            bb_colours: Default::default(),
            material: Default::default(),
        }
    }
}

impl SetPosition<SquareIndex64, PieceWithColour> for Position64 {
    fn set_square_to_piece(&mut self, square: SquareIndex64, piece: PieceWithColour) {
        let current_piece = self.squares[square as usize];

        // Update the hash key.
        self.hash_key ^= ZOBRIST_NUMBERS.piece_square[current_piece.colour as usize]
            [current_piece.piece_type as usize][square as usize];
        self.hash_key ^= ZOBRIST_NUMBERS.piece_square[piece.colour as usize]
            [piece.piece_type as usize][square as usize];

        let mask = Bitboard::from_single_square(square);
        self.bb_pieces[current_piece.piece_type as usize] ^= mask;
        self.bb_colours[current_piece.colour as usize] ^= mask;
        self.bb_pieces[piece.piece_type as usize] ^= mask;
        self.bb_colours[piece.colour as usize] ^= mask;

        // Set the piece on the square.
        self.squares[square as usize] = piece;

        // Update the material.
        self.material[current_piece.colour as usize] -= current_piece.piece_type.evaluation_value();
        self.material[piece.colour as usize] += piece.piece_type.evaluation_value();
    }

    fn remove_from_square(&mut self, square: SquareIndex64) {
        self.set_square_to_piece(square, PieceWithColour::empty());
    }

    fn square_piece(&self, square: SquareIndex64) -> PieceWithColour {
        self.squares[square as usize]
    }
}

impl HasCastlingRights for Position64 {
    fn can_castle(&self, castling: Castling) -> bool {
        self.castling_rights.0[castling as usize]
    }

    fn set_castling(&mut self, castling: Castling, can_castle: bool) {
        let index = castling as usize;
        if self.castling_rights.0[index] != can_castle {
            self.hash_key ^= ZOBRIST_NUMBERS.castling_rights[index];
        }
        self.castling_rights.0[index] = can_castle;
    }
}

impl Position64 {
    pub fn get_side_to_move(&self) -> Colour {
        self.side_to_move
    }

    pub fn set_ep_square(&mut self, square: Bitboard) {
        // We can only use trailing_zeros() because it's a non-zero bitboard.
        if self.ep_square != 0 {
            self.hash_key ^= ZOBRIST_NUMBERS.ep_file
                [file(self.ep_square.trailing_zeros() as SquareIndex64) as usize];
        }

        if square != 0 {
            self.hash_key ^=
                ZOBRIST_NUMBERS.ep_file[file(square.trailing_zeros() as SquareIndex64) as usize];
        }
        self.ep_square = square;
    }
}

impl Hashable for Position64 {
    fn hash_key(&self) -> ZobristNumber {
        self.hash_key
    }
}

impl Debug for Position64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sep = "+-".repeat(8) + "+\n";
        write!(f, "{}", &sep)?;
        for i in (0..8).rev().map(|x| x * 8) {
            write!(f, "|")?;
            for sq in &self.squares[i..i + 8] {
                write!(f, "{}", sq.to_algebraic_notation())?;
                write!(f, "|")?;
            }
            write!(f, "{}", "\n")?;
            write!(f, "{}", &sep)?;
        }
        Ok(())
    }
}

impl FromStr for Position64 {
    type Err = FenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut pos = Position64::default();
        let fen: Fen = s.parse()?;

        // Set the pieces.
        fen.set_piece_placements(&mut pos)?;

        // Set the side to move.
        pos.side_to_move = match fen.side_to_move {
            'w' => Colour::White,
            'b' => Colour::Black,
            x => return Err(FenError::InvalidColourToMove(x.to_string())),
        };

        // Set the castling rights.
        if fen.castling != "-" {
            for c in fen.castling.chars() {
                match c {
                    'K' => pos.set_castling(Castling::WhiteKingSide, true),
                    'Q' => pos.set_castling(Castling::WhiteQueenSide, true),
                    'k' => pos.set_castling(Castling::BlackKingSide, true),
                    'q' => pos.set_castling(Castling::BlackQueenSide, true),
                    _ => return Err(FenError::InvalidCastlingInfo(fen.castling.to_string())),
                }
            }
        }

        // Set the en passant square.
        if fen.ep_square != "-" {
            let ep_square = SquareIndex64::sq_from_algebraic_notation(fen.ep_square.as_str())?;
            let ep_bitboard = Bitboard::from_single_square(ep_square);
            pos.set_ep_square(ep_bitboard);
        }

        // Set the halfmove clock.
        pos.halfmove_clock = fen
            .halfmove
            .parse::<u32>()
            .or_else(|_| Err(FenError::InvalidDigit(fen.halfmove.to_string())))?;

        // Set the fullmove number.
        pos.fullmove_number = fen
            .fullmove
            .parse::<u32>()
            .or_else(|_| Err(FenError::InvalidDigit(fen.fullmove.to_string())))?;

        Ok(pos)
    }
}

impl Display for Position64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut empty_count = 0;
        for rank in (0..8).rev() {
            for file in 0..8 {
                let sq = SquareIndex64::from_rank_and_file(rank, file);
                let piece = self.square_piece(sq);
                if piece.piece_type == PieceType::Empty {
                    empty_count += 1;
                } else {
                    if empty_count > 0 {
                        write!(f, "{}", empty_count)?;
                        empty_count = 0;
                    }
                    write!(f, "{}", piece.to_algebraic_notation())?;
                }
            }
            if empty_count > 0 {
                write!(f, "{}", empty_count)?;
                empty_count = 0;
            }
            write!(f, "{}", if rank > 0 { "/" } else { " " })?;
        }

        write!(f, "{} ", self.side_to_move.to_algebraic_notation())?;

        if self.castling_rights.0.iter().all(|&x| !x) {
            write!(f, "- ")?;
        } else {
            if self.can_castle(Castling::WhiteKingSide) {
                write!(f, "K")?;
            }
            if self.can_castle(Castling::WhiteQueenSide) {
                write!(f, "Q")?;
            }
            if self.can_castle(Castling::BlackKingSide) {
                write!(f, "k")?;
            }
            if self.can_castle(Castling::BlackQueenSide) {
                write!(f, "q")?;
            }
            write!(f, " ")?;
        }

        if self.ep_square == 0 {
            write!(f, "- ")?;
        } else {
            let ep_square_index = self.ep_square.to_single_square().or(Err(Error))?;
            write!(f, "{} ", ep_square_index.sq_to_algebraic_notation())?;
        }

        write!(f, "{} {}", self.halfmove_clock, self.fullmove_number)?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_opposite_colour() {
        assert_eq!(Colour::Black, Colour::White.opposite());
        assert_eq!(Colour::White, Colour::Black.opposite());
        assert_eq!(Colour::None, Colour::None.opposite());
    }

    #[test]
    fn test_from_str() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let pos: Position64 = fen.parse().unwrap();
        assert_eq!(
            pos.squares[56],
            PieceWithColour::new(PieceType::Rook, Colour::Black)
        );
        assert_eq!(
            pos.squares[62],
            PieceWithColour::new(PieceType::Knight, Colour::Black)
        );
        assert_eq!(
            pos.squares[0],
            PieceWithColour::new(PieceType::Rook, Colour::White)
        );
        assert_eq!(
            pos.squares[6],
            PieceWithColour::new(PieceType::Knight, Colour::White)
        );
        assert_eq!(pos.side_to_move, Colour::White);
        assert!(pos.castling_rights.0.iter().all(|&x| x));
        assert_eq!(pos.ep_square, 0);
        assert_eq!(pos.halfmove_clock, 0);
        assert_eq!(pos.fullmove_number, 1);
        assert_eq!(24000, pos.material[Colour::White as usize]);
        assert_eq!(24000, pos.material[Colour::Black as usize]);
    }

    #[test]
    fn test_display() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let pos: Position64 = fen.parse().unwrap();
        assert_eq!(pos.to_string(), fen);
    }
}
