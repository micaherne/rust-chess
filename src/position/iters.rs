use super::{
    movegen_simple::SquareAndPiece, piece_colour, piece_type, Colour, Piece, PieceType, Position,
    SquareIndex, BISHOP, BLACK, EMPTY, KING, KNIGHT, PAWN, QUEEN, ROOK, WHITE,
};

#[derive(Default, Debug)]
pub struct SquareIterator {
    next: SquareIndex,
}

impl Iterator for SquareIterator {
    type Item = SquareIndex;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next > 0x77 {
            return None;
        }
        let result = Some(self.next);
        if self.next & 7 == 7 {
            self.next += 9;
        } else {
            self.next += 1;
        }
        result
    }
}

pub fn square_iterator() -> impl Iterator<Item = SquareIndex> {
    SquareIterator::default()
}

pub fn piece_iterator<'a>(position: &'a Position) -> impl 'a + Iterator<Item = SquareAndPiece> {
    square_iterator()
        .map(|sq| (sq, position.square_piece(sq)))
        .filter(|x| x.1 != EMPTY)
}

trait CheckColour {
    fn is_white(&self) -> bool {
        self.is_colour(WHITE)
    }
    fn is_black(&self) -> bool {
        self.is_colour(BLACK)
    }
    fn is_colour(&self, colour: Colour) -> bool;
}

impl CheckColour for Piece {
    fn is_colour(&self, colour: Colour) -> bool {
        piece_colour(*self) == Some(colour)
    }
}

impl CheckColour for SquareAndPiece {
    fn is_colour(&self, colour: Colour) -> bool {
        self.1.is_colour(colour)
    }
}

trait CheckPieceType {
    fn is_piece_type(&self, check_type: PieceType) -> bool;
    fn is_empty(&self) -> bool {
        self.is_piece_type(EMPTY)
    }
    fn is_pawn(&self) -> bool {
        self.is_piece_type(PAWN)
    }
    fn is_rook(&self) -> bool {
        self.is_piece_type(ROOK)
    }
    fn is_knight(&self) -> bool {
        self.is_piece_type(KNIGHT)
    }
    fn is_bishop(&self) -> bool {
        self.is_piece_type(BISHOP)
    }
    fn is_queen(&self) -> bool {
        self.is_piece_type(QUEEN)
    }
    fn is_king(&self) -> bool {
        self.is_piece_type(KING)
    }
}

impl CheckPieceType for Piece {
    fn is_piece_type(&self, check_type: PieceType) -> bool {
        piece_type(*self) == check_type
    }
}

impl CheckPieceType for SquareAndPiece {
    fn is_piece_type(&self, check_type: PieceType) -> bool {
        self.1.is_piece_type(check_type)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        fen::STARTPOS_FEN,
        position::{get_piece, BLACK, ROOK, WHITE},
    };

    use super::*;

    #[test]
    fn test_square_iterator() {
        let squares: Vec<SquareIndex> = square_iterator().collect();
        assert_eq!(64, squares.len());
        assert_eq!(0, squares[0]);
        assert_eq!(16, squares[8]);
        assert_eq!(0x77, squares[63]);
    }

    #[test]
    fn test_piece_iterator() {
        let position: Position = STARTPOS_FEN.into();
        let pieces: Vec<SquareAndPiece> = piece_iterator(&position).collect();
        assert_eq!(32, pieces.len());
        assert_eq!(0, pieces[0].0);
        assert_eq!(get_piece(ROOK, WHITE), pieces[0].1);
        assert_eq!(0x70, pieces[24].0);
        assert_eq!(get_piece(ROOK, BLACK), pieces[24].1);

        let p2: Vec<SquareAndPiece> = piece_iterator(&position).filter(|p| p.is_white()).collect();
        assert_eq!(16, p2.len());
        let p3: Vec<SquareAndPiece> = piece_iterator(&position)
            .filter(|p| p.is_knight())
            .collect();
        assert_eq!(4, p3.len());
    }
}
