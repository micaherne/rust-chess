use std::fmt::Debug;

use regex::Regex;

use crate::fen::{Fen, parse_fen};

pub struct Board {
    pub squares: [u32; 128],
    pub whiteToMove: bool,
    pub castling: [bool; 4],
    pub epSquare: Square,
    pub halfmoveClock: u32,
    pub fullmoveNumber: u32,
    undo_stack: Vec<MoveUndo>
}

/// The type of a piece, without colour, e.g. knight, bishop etc.
pub type PieceType = u32;

/// The colour of a piece.
pub type Colour = u32;

/// A piece with colour, e.g. a black knight.
pub type Piece = u32;

pub const BLACK: Colour = 16;
pub const WHITE: Colour = 32;

const EMPTY: PieceType = 0;
const PAWN: PieceType = 1;
const ROOK: PieceType = 2;
const KNIGHT: PieceType = 3;
const BISHOP: PieceType = 4;
const QUEEN: PieceType = 5;
const KING: PieceType = 6;

const STARTPOS_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

pub fn get_piece(piece_type: PieceType, colour: Colour) -> Piece {
    piece_type | colour
}

pub fn piece_colour(piece: Piece) -> Option<Colour> {
    if piece & BLACK != 0 {
        Some(BLACK)
    } else if piece & WHITE != 0 {
        Some(WHITE)
    } else {
        None
    }
}

pub fn char_to_piece_type(c: char) -> Option<PieceType> {
    match c.to_ascii_lowercase() {
        'p' => Some(PAWN),
        'r' => Some(ROOK),
        'n' => Some(KNIGHT),
        'b' => Some(BISHOP),
        'q' => Some(QUEEN),
        'k' => Some(KING),
        _ => None,
    }
}

pub fn piece_type_to_char(p: PieceType) -> Option<char> {
    match p {
        EMPTY =>  Some(' '),
        PAWN => Some('p'),
        ROOK => Some('r'),
        KNIGHT =>  Some('n'),
        BISHOP => Some('b'),
        QUEEN => Some('q'),
        KING => Some('k'),
        _ => None
    }
}

pub fn char_to_colour(c: char) -> Option<Colour> {
    match c.to_ascii_lowercase() {
        'b' => Some(BLACK),
        'w' => Some(WHITE),
        _ => None,
    }
}

pub fn str_to_square(s: &str) -> Option<Square> {
    let lower = s.to_ascii_lowercase();
    let bytes = lower.as_bytes();
    let file = bytes[0];
    let file_no = file - 97;
    let rank = bytes[1];
    let rank_no = rank - 0x31;
    if file > 7 || rank_no > 7 {
        None
    } else {
        Some(Square(file_no, rank_no))
    }
}

/// Converts a character to a piece with colour.
///
/// e.g. char_to_piece('n') = black knight
///      char_to_piece('N') = white knight
///
fn char_to_piece(c: char) -> Option<u32> {
    let piece_type = char_to_piece_type(c);

    match piece_type {
        None => None,
        Some(pt) => {
            let is_white = c == c.to_ascii_uppercase();

            let colour = if is_white { WHITE } else { BLACK };

            let piece = pt | colour;

            Some(piece)
        }
    }
}

fn piece_to_char(p: Piece) -> Option<char> {
    let piece_type = p & 0xF;
    let result = piece_type_to_char(piece_type);
    match result {
        None => None,
        Some(chr) => {
            if p & WHITE != 0 {
                Some(chr.to_ascii_uppercase())
            } else {
                Some(chr.to_ascii_lowercase())
            }
        }
    }
    
}

pub fn square_index(square: Square) -> usize {
    println!("{:#?}", square);
    return (square.0 + 16 * square.1).into();
}

pub fn validate_long_algebraic_move(text: &str) -> bool {
    let re = Regex::new("([a-h][1-8]){2}[rnbq]").unwrap();
    re.is_match(text)
}

impl Board {
    pub fn set_from_fen(&mut self, fen: Fen) {
        self.squares = [EMPTY; 128];

        self.set_pieces_from_fen(&fen.piecePlacement).unwrap();
        self.castling = fen.castling;
        self.epSquare = fen.epSquare;
        self.halfmoveClock = fen.halfmoveClock;
        self.fullmoveNumber = fen.fullmoveNumber;
    }

    pub fn set_startpos(&mut self) {
        self.set_from_fen(parse_fen(STARTPOS_FEN).unwrap());
    }

    pub fn make_moves(&mut self, moves: &Vec<LongAlgebraicNotationMove>) {
        for mv in moves {
            let from = mv.from_square();
            let to = mv.to_square();
            let queening_piece = mv.queening_piece();
            self.make_move(from, to, queening_piece)
        }
    }

    pub fn make_move(&mut self, from: Square, to: Square, queening_piece: Option<PieceType>) {
        let from_index = square_index(from);
        let to_index = square_index(to);
        let captured_piece = self.squares[to_index];
        let undo = MoveUndo {
            from,
            to,
            captured_piece,
            castling: self.castling,
            // TODO: This is stupid - implement copy on Square.
            ep_square: self.epSquare,
            halfmove_clock: self.halfmoveClock
        };
        self.undo_stack.push(undo);

        match queening_piece {
            None => {
                self.squares[to_index] = self.squares[from_index];
            },
            Some(piece_type) => {
                let colour = piece_colour(self.squares[from_index]).unwrap();
                self.squares[to_index] = get_piece(piece_type, colour);
            }
        }
        self.squares[from_index] = EMPTY;

        // TODO: En passent captures, castling.
    }

    fn colour_to_move(&self) -> Colour {
        if self.whiteToMove {
            WHITE
        } else {
            BLACK
        }
    }

    fn set_pieces_from_fen(&mut self, fen_part: &str) -> Result<(), &str> {
        let mut i = FenIndexProvider::new();
        for c in fen_part.chars() {
            if c == '/' {
                // Assert that we're at the end of a row.
                continue;
            } else if c.is_numeric() {
                let x = c.to_digit(10).unwrap() as usize;
                i.skip(x);
                continue;
            }

            let piece = char_to_piece(c);

            match piece {
                Some(val) => {
                    self.squares[i.next()] = val;
                }
                None => return Err("Invalid piece type"),
            }
        }
        Ok(())
    }
}

impl Debug for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        let sep = "+-".repeat(8) + "+\n";
        s.push_str(&sep);
        for i in (0x00..0x80).step_by(0x10).rev() {
            s.push_str("|");
            for sq in self.squares[i..i+8].iter().map(|s| piece_to_char(*s)) {
                s.push(sq.unwrap());
                s.push_str("|");
            }
            s.push_str("\n");
            s.push_str(&sep);
        }
        writeln!(f, "{}", s)
    }
}

impl Default for Board {
    fn default() -> Self {
        Board {
            squares: [EMPTY; 128],
            whiteToMove: true,
            castling: [true, true, true, true],
            epSquare: Square(0, 0),
            halfmoveClock: 0,
            fullmoveNumber: 0,
            undo_stack: Vec::new()
        }
    }
}

struct MoveUndo {
    from: Square,
    to: Square,
    captured_piece: Piece,
    ep_square: Square,
    halfmove_clock: u32,
    castling: [bool; 4]
}

struct FenIndexProvider {
    current: usize,
    valid: bool,
}

// TODO: Make this a proper iterator.
impl FenIndexProvider {
    fn new() -> Self {
        return FenIndexProvider {
            current: 0x70,
            valid: true,
        };
    }

    fn reset(&mut self) {
        self.current = 0x70;
        self.valid = true;
    }

    fn next(&mut self) -> usize {
        if !self.valid {
            panic!("All squares given")
        }
        let result = self.current;

        self.current += 1;

        if self.current & 0x88 != 0 {
            if self.current < 0x18 {
                self.valid = false;
            } else {
                self.current -= 0x18;
            }
        }
        result
    }

    fn skip(&mut self, count: usize) {
        if count > 8 {
            panic!("Can't skip more than 8 spaces");
        }

        self.current += count;

        if self.current & 0x88 != 0 {
            self.current -= 0x18;
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Square(pub u8, pub u8);

trait Move {
    fn from_square(&self) -> Square;
    fn to_square(&self) -> Square;
    fn queening_piece(&self) -> Option<u32>;
}

#[derive(Debug)]
pub struct LongAlgebraicNotationMove {
    pub text: String,
}

impl Move for LongAlgebraicNotationMove {
    fn from_square(&self) -> Square {
        let lower = self.text.to_ascii_lowercase();
        let bytes = lower.as_bytes();
        let file = bytes[0];
        let file_no = file - 97;
        let rank = bytes[1];
        let rank_no = rank - 0x31;
        Square(file_no, rank_no)
    }

    fn to_square(&self) -> Square {
        let lower = self.text.to_ascii_lowercase();
        let bytes = lower.as_bytes();
        let file = bytes[2];
        let file_no = file - 97;
        let rank = bytes[3];
        let rank_no = rank - 0x31;
        Square(file_no, rank_no)
    }
    fn queening_piece(&self) -> Option<PieceType> {
        let chars: Vec<char> = self.text.chars().collect();
        let q = chars.get(4);
        match q {
            None => None,
            Some(i) => char_to_piece_type(*i),
        }
    }
}

impl LongAlgebraicNotationMove {
    pub fn from_text(text: String) -> LongAlgebraicNotationMove {
        return LongAlgebraicNotationMove { text };
    }
}

// enum Piece {
//     Empty = 0,
//     Pawn = 1,
//     Rook = 2,
//     Knight = 3,
//     Bishop = 4,
//     Queen = 5,
//     King = 6
// }

// enum Colour {
//     Black = 0,
//     White = 16
// }

// enum ColouredPiece {
//     Empty = Piece::Empty,
//     BlackPawn = Piece::Pawn | Colour::Black,
//     BlackRook = Piece::Rook | Colour::Black,
//     BlackKnight = Piece::Knight | Colour::Black,
//     BlackBishop = Piece::Bishop | Colour::Black,
//     BlackQueen = Piece::Queen | Colour::Black,
//     BlackKing = Piece::King | Colour::Black,
//     WhitePawn = Piece::Pawn | Colour::White,
//     WhiteRook = Piece::Rook | Colour::White,
//     WhiteKnight = Piece::Knight | Colour::White,
//     WhiteBishop = Piece::Bishop | Colour::White,
//     WhiteQueen = Piece::Queen | Colour::White,
//     WhiteKing = Piece::King | Colour::White,
// }

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_set_pieces_from_fen() {
        let mut b = Board::default();
        b.set_pieces_from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
            .unwrap();
        assert!(b.squares[0x77] == BLACK | ROOK);
        assert!(b.squares[0x07] == WHITE | ROOK);
        assert!(b.squares[0x70] == BLACK | ROOK);
        assert!(b.squares[0x00] == WHITE | ROOK);
    }
}
