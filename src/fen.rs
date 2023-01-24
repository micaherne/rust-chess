use regex::Regex;

use crate::board::{Colour, Square, char_to_piece_type, char_to_colour, str_to_square, CastlingRights, BLACK, WHITE, BoardSide};

pub const STARTPOS_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[derive(Debug)]
pub struct Fen {
    pub piecePlacement: String,
    pub sideToMove: Colour,
    pub castling: CastlingRights,
    pub epSquare: Square,
    pub halfmoveClock: u32,
    pub fullmoveNumber: u32,
}

pub fn parse_fen(fen: &str) -> Result<Fen, FenError> {
    let cnt = fen.split_ascii_whitespace().count();
    if cnt != 6 {
        return Err(FenError::IncorrectNumberOfParts);
    }

    let mut parts = fen.split_ascii_whitespace();
    let piece_placements = parts.next().unwrap();

    let mut count = 0;
    let mut line_count = 0;
    let mut slash_count = 0;
    for chr in piece_placements.chars() {
        if chr.is_numeric() {
            let skip_num = chr.to_digit(10);
            match skip_num {
                Some(empty_count) => {
                    if empty_count > 8 {
                        return Err(FenError::InvalidDigit);
                    }
                    count += empty_count;
                    line_count += empty_count;
                }
                None => return Err(FenError::InvalidDigit),
            }
        } else if chr == '/' {
            slash_count += 1;
            if line_count != 8 {
                return Err(FenError::InvalidLine)
            }
            line_count = 0;
        } else {
            let piece = char_to_piece_type(chr);
            match piece {
                Some(_) => (),
                None => return Err(FenError::InvalidPiece)
            }
            line_count += 1;
        }
    }

    let side_to_move = parts.next().unwrap();
    if side_to_move.len() != 1 {
        return Err(FenError::InvalidColourToMove);
    }

    let mut x = side_to_move.chars();
    let colour_to_move = match x.next() {
        Some(colour) => match(char_to_colour(colour)) {
            Some(clr) => clr,
            None => return Err(FenError::InvalidColourToMove)
        },
        None => return Err(FenError::InvalidColourToMove)
    };

    let re = Regex::new("^([KQkq]+|-)$").unwrap();
    let castling_chars = parts.next().unwrap();
    if !re.is_match(castling_chars) {
        return Err(FenError::InvalidCastlingInfo);
    }

    let mut castling_rights = CastlingRights::new();

    for c in castling_chars.chars() {
        let colour = match c.to_ascii_lowercase() == c {
            true => BLACK,
            false => WHITE,
        };
        let side = match c.to_ascii_lowercase() {
            'k' => BoardSide::Kingside,
            'q' => BoardSide::Queenside,
            _ => return Err(FenError::InvalidCastlingInfo)
        };
        castling_rights.allow(colour, side);
        
    }

    let ep_square_str = parts.next().unwrap();
    let ep_square = match ep_square_str {
        "-" => Square(0, 0),
        x => {
            let ep = str_to_square(x);
            match ep {
                Some(sq) => sq,
                None => return Err(FenError::InvalidSquare)
            }
        }
    };
    
    let halfmove_str = parts.next().unwrap();
    for halfmove_char in halfmove_str.chars() {
        if !halfmove_char.is_numeric() {
            return Err(FenError::InvalidDigit);
        }
    }

    let halfmove = halfmove_str.parse::<u32>().unwrap();

    let fullmove_str = parts.next().unwrap();
    for fullmove_char in fullmove_str.chars() {
        if !fullmove_char.is_numeric() {
            return Err(FenError::InvalidDigit);
        }
    }

    let fullmove = fullmove_str.parse::<u32>().unwrap();

    Ok(Fen {
        piecePlacement: String::from(piece_placements),
        sideToMove: colour_to_move,
        castling: castling_rights,
        epSquare: ep_square,
        halfmoveClock: halfmove,
        fullmoveNumber: fullmove
    })
}

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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_fen() {
        let r = parse_fen(STARTPOS_FEN);
        match r {
            Ok(fen) => println!("{:#?}", fen),
            Err(_fen_error) => panic!("Parsing failed")
        }
    }
}
