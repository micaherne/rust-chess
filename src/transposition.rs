use std::{collections::HashMap, time::SystemTime};

use rand::RngCore;

use crate::{
    position0x88::{
        evaluate::Score,
        movegen::{Move, PIECE_TYPES_COUNT},
    },
    search::Depth,
};

#[derive(Debug, Clone, Copy)]
pub struct TranspositionItem {
    pub key: ZobristNumber,
    pub best_move: Option<Move>,
    pub depth: Depth,
    pub score: Score,
    pub node_type: NodeType,
    pub created: SystemTime,
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
        let mut i = 0;
        let mut actual_size = 1 << i;
        while actual_size < requested_size {
            i += 1;
            actual_size <<= 1;
        }
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

    pub fn probe(&self, key: ZobristNumber) -> Option<TranspositionItem> {
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
    pub piece_square: [ZobristPieceSquare; PIECE_TYPES_COUNT],
    pub black_to_move: ZobristNumber,
    pub castling_rights: [ZobristNumber; 4],
    pub ep_file: [ZobristNumber; 8],
}

impl ZobristNumbers {
    pub fn init() -> ZobristNumbers {
        let mut rng = rand::thread_rng();
        let mut result = ZobristNumbers {
            piece_square: [[0; 64]; PIECE_TYPES_COUNT],
            black_to_move: 0,
            castling_rights: [0; 4],
            ep_file: [0; 8],
        };
        for piece in 1..PIECE_TYPES_COUNT {
            for square in 0..64 {
                result.piece_square[piece][square] = rng.next_u64();
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
        };

        tt1.store(item1);
        let retrieved_item1 = tt1.items.get(&71).unwrap();
        assert_eq!(5 * 128 + 71, retrieved_item1.key);
        assert_eq!(1, tt1.items.len());
        assert_eq!(3, retrieved_item1.depth);

        let probed1 = tt1.probe(5 * 128 + 71).unwrap();
        assert_eq!(5 * 128 + 71, probed1.key);
        assert!(tt1.probe(71).is_none());

        let tt2 = TranspositionTable::new(1_000_000);
        assert_eq!(1 << 20, tt2.actual_size);
        assert_eq!((1 << 20) - 1, tt2.key_mask);
    }
}
