use std::{collections::HashMap, time::SystemTime};

use rand::RngCore;

use crate::{
    position::PIECE_COUNT,
    position64::{evaluate::Score, moves::Move, Colour},
    search::Depth,
};

pub trait Hashable {
    fn hash_key(&self) -> ZobristNumber;
}

#[derive(Debug, Clone)]
pub struct TranspositionItem {
    pub key: ZobristNumber,
    pub best_move: Option<Move>,
    pub depth: Depth,
    pub score: Score,
    pub node_type: NodeType,
    pub created: SystemTime,
    #[cfg(debug_assertions)]
    pub fen: String,
}

#[derive(Debug)]
pub struct TranspositionTable {
    _requested_size: usize,
    actual_size: usize,
    key_mask: ZobristNumber,
    items: HashMap<ZobristNumber, TranspositionItem>,
}

impl TranspositionTable {
    pub fn new(requested_size: usize) -> Self {
        let actual_size = requested_size.next_power_of_two();

        let key_mask = (actual_size - 1) as ZobristNumber;
        let items = HashMap::with_capacity(actual_size);
        Self {
            _requested_size: requested_size,
            actual_size,
            key_mask,
            items,
        }
    }
    pub fn store(&mut self, item: TranspositionItem) {
        self.items.insert(self.index(item.key), item);
    }

    #[inline]
    pub fn index(&self, key: ZobristNumber) -> ZobristNumber {
        key & self.key_mask
    }

    pub fn fetch(&self, key: ZobristNumber) -> Option<TranspositionItem> {
        let result = self.items.get(&self.index(key));
        if result.is_some() {
            let r = result.unwrap();
            if r.key == key {
                return Some(r.clone());
            } else {
                return None;
            }
        }
        None
    }

    pub fn usage(&self) -> f64 {
        if self.items.len() == 0 {
            0f64
        } else {
            self.actual_size as f64 / self.items.len() as f64
        }
    }
}

pub type ZobristNumber = u64;
pub type ZobristPieceSquare = [ZobristNumber; 64];

#[derive(Debug, Clone, Copy)]
pub enum NodeType {
    PV,
    All,
    Cut,
}

#[derive(Clone, Copy)]
pub struct ZobristNumbers {
    pub piece_square: [[ZobristPieceSquare; PIECE_COUNT]; 3],
    pub black_to_move: ZobristNumber,
    pub castling_rights: [ZobristNumber; 4],
    pub ep_file: [ZobristNumber; 8],
}

impl ZobristNumbers {
    pub fn init() -> ZobristNumbers {
        let mut rng = rand::thread_rng();
        let mut result = ZobristNumbers {
            piece_square: [[[0; 64]; PIECE_COUNT]; 3],
            black_to_move: 0,
            castling_rights: [0; 4],
            ep_file: [0; 8],
        };
        for piece in 1..PIECE_COUNT {
            for square in 0..64 {
                for colour in [Colour::White, Colour::Black, Colour::None] {
                    result.piece_square[colour as usize][piece][square] = rng.next_u64();
                }
            }
        }
        result.black_to_move = rng.next_u64();
        (0..4).for_each(|i| result.castling_rights[i] = rng.next_u64());
        (0..8).for_each(|i| result.ep_file[i] = rng.next_u64());

        result
    }
}

#[cfg(test)]
mod test {

    use crate::position64::{moves::MakeMove, Position64};

    use super::*;

    #[test]
    fn test_transposition_table() {
        let mut tt1 = TranspositionTable::new(100);
        assert_eq!(128, tt1.actual_size);
        assert_eq!(127, tt1.key_mask);

        let item1 = TranspositionItem {
            key: 5 * 128 + 71,
            best_move: None,
            depth: 3,
            score: -288,
            node_type: NodeType::PV,
            created: SystemTime::now(),
            #[cfg(debug_assertions)]
            fen: "".to_owned(),
        };

        tt1.store(item1);
        let retrieved_item1 = tt1.items.get(&71).unwrap();
        assert_eq!(5 * 128 + 71, retrieved_item1.key);
        assert_eq!(1, tt1.items.len());
        assert_eq!(3, retrieved_item1.depth);

        let probed1 = tt1.fetch(5 * 128 + 71).unwrap();
        assert_eq!(5 * 128 + 71, probed1.key);
        assert!(tt1.fetch(71).is_none());

        let tt2 = TranspositionTable::new(1_000_000);
        assert_eq!(1 << 20, tt2.actual_size);
        assert_eq!((1 << 20) - 1, tt2.key_mask);
    }

    #[test]
    fn test_hash() {
        let pos1: Position64 = "r4k2/pppR2pp/2pbp3/6P1/5B2/2N2P2/PP5P/2K1R3 w - - 0 21"
            .parse()
            .unwrap();
        let pos2: Position64 = "r4k2/pppr2pp/2pbp3/6P1/5B2/2N2P2/PP5P/2K1R3 w - - 0 21"
            .parse()
            .unwrap();
        assert_ne!(pos1.hash_key(), pos2.hash_key());

        let mut pos1: Position64 = "rnbqkbnr/1ppppppp/8/8/p6P/8/PPPPPPP1/RNBQKBNR b KQkq - 0 3"
            .parse()
            .unwrap();
        let pos2: Position64 = "rnbqkbnr/1ppppppp/8/8/p6P/8/PPPPPPP1/RNBQKBNR b Qkq - 0 3"
            .parse()
            .unwrap();
        assert_ne!(pos1.hash_key(), pos2.hash_key());

        let pre1 = pos1.hash_key();
        let undo = pos1.make_move(Move::new(7, 23, None));
        pos1.undo_move(undo);
        assert_eq!(pre1, pos1.hash_key());

        let mut pos1: Position64 =
            "r2qkb1r/pp2nppp/8/2pNp1B1/P1BnP3/3P3P/1PP2PP1/R2bK2R b KQkq a3 0 1"
                .parse()
                .unwrap();
        let pre1 = pos1.hash_key();
        let undo = pos1.make_move(Move::new(48, 40, None));
        pos1.undo_move(undo);
        println!("{}", pos1);
        assert_eq!(pre1, pos1.hash_key());
    }
}
