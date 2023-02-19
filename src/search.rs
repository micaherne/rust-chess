use std::{
    sync::mpsc::{Receiver, Sender},
    thread, time::SystemTime,
};

use crate::{
    messages::{GoSubcommand, InfoMessage, InputMessage, OutputMessage, ScoreInfo},
    position0x88::{
        evaluate::{evaluate, Score, CHECKMATE_SCORE_MAX},
        movegen::{generate_moves, side_to_move_in_check, Move},
        notation::{make_move, undo_move, LongAlgebraicNotationMove},
        Position,
    }, transposition::{NodeType, TranspositionTable, TranspositionItem},
};

pub type Depth = i16;

type Line = Vec<Move>;

const TRANSPOSITION_TABLE_SIZE: usize = 1_000_000;

#[derive(Debug)]
pub struct SearchTree {
    position: Position,
    search_depth: Depth,
    transposition_table: TranspositionTable,
    pv: Line,
    nodes_searched: usize,
    search_start: SystemTime,
    sender: Sender<OutputMessage>,
    _receiver: Receiver<InputMessage>,
}

impl SearchTree {
    pub fn new(
        position: Position,
        depth: Depth,
        sender: Sender<OutputMessage>,
        receiver: Receiver<InputMessage>,
    ) -> Self {
        Self {
            position,
            search_depth: depth,
            transposition_table: TranspositionTable::new(TRANSPOSITION_TABLE_SIZE),
            pv: vec![],
            nodes_searched: 0,
            search_start: SystemTime::now(),
            sender,
            _receiver: receiver,
        }
    }
    pub fn start_search_thread(
        position: Position,
        _commands: &[GoSubcommand],
        output_sender: Sender<OutputMessage>,
        input_receiver: Receiver<InputMessage>,
    ) {
        thread::spawn(move || {
            let mut tree = SearchTree::new(position, 5, output_sender, input_receiver);
            tree.search();
        });
    }
    
    // TODO: This doesn't deal with sending the ponder move back to the engine - it would go direct to
    // the uci thread if we sent it here.
    pub fn search(&mut self) -> LongAlgebraicNotationMove {
        let mut pline: Line = vec![Move::default(); self.search_depth as usize];

        self.nodes_searched = 0;
    
        self.search_ab(
            Score::MIN / 2,
            Score::MAX / 2, // As -MAX is lower than MIN?
            self.search_depth,
            &mut pline,
        );
    
        let pv_algebraic: Vec<LongAlgebraicNotationMove> = pline.iter().map(|m| m.into()).collect();
    
        let alg_move = pv_algebraic[0].clone();
    
        self
            .sender
            .send(OutputMessage::BestMove(alg_move.clone(), None))
            .unwrap();
    
        #[cfg(debug_assertions)]
        println!("Thread exiting");
    
        return alg_move;
    }
    
    fn search_ab(
        &mut self,
        alpha: Score,
        beta: Score,
        depthleft: Depth,
        pline: &mut Line,
    ) -> Score {
        self.nodes_searched += 1;

        let mut line: Line = vec![];
    
        // This is a hack to let us modify alpha without changing it to a mutable parameter.
        // TODO: Just sort out the parameters and calls.
        let mut alpha_local = alpha;

        // Node type to insert into transposition table.
        let mut tt_node_type = NodeType::All;

        let from_tt = self.transposition_table.probe(self.position.hash_key());
        if from_tt.is_some() {
            let from_tt_val = from_tt.unwrap();
            if from_tt_val.depth >= depthleft {
                match from_tt_val.node_type {
                    NodeType::PV => return from_tt_val.score,
                    NodeType::All => if from_tt_val.score <= alpha { return alpha; },
                    NodeType::Cut => if from_tt_val.score > beta { return beta; },
                }
            }
        }
    
        if depthleft == 0 {
            let eval = evaluate(&self.position);
            let tt_item = TranspositionItem {
                key: self.position.hash_key(),
                best_move: None,
                depth: depthleft,
                score: eval,
                node_type: NodeType::PV,
                created: SystemTime::now()
            };
            self.transposition_table.store(tt_item);
            return eval;
        }
    
        let moves = generate_moves(&self.position);
    
        // Test for checkmate / stalemate.
        if moves.len() == 0 {
            if side_to_move_in_check(&self.position) {
                // Bigger for closer mates.
                return  - (depthleft as Score + CHECKMATE_SCORE_MAX);
            } else {
                return 0;
            }
        }
    
        for mv in moves {
            let undo = make_move(&mut self.position, mv.from_index, mv.to_index, mv.queening_piece);
            let move_score = - self.search_ab(
                -beta,
                -alpha_local,
                depthleft - 1,
                &mut line,
            );
            undo_move(&mut self.position, undo);
    
            if move_score >= beta {
                let tt_item = TranspositionItem {
                    key: self.position.hash_key(),
                    best_move: None,
                    depth: depthleft,
                    score: beta,
                    node_type: NodeType::Cut,
                    created: SystemTime::now()
                };
                self.transposition_table.store(tt_item);
                return beta;
            }
    
            if move_score > alpha_local {
                alpha_local = move_score;

                tt_node_type = NodeType::PV;
    
                // If alpha is raised, update the PV.
                pline.clear();
                pline.push(mv);
                pline.extend(&line);
    
                let pv_algebraic: Vec<LongAlgebraicNotationMove> =
                    pline.iter().map(|m| m.into()).collect();
    
                // Send info output.
                if depthleft == self.search_depth {
                    // Pull it into the search data.
                    self.pv = pline.iter().map(|m| m.to_owned()).collect();
    
                    // TODO: This should negate the score if black is to move I think.
                    let score = InfoMessage::Score(vec![
                        ScoreInfo::Centipawns(move_score)
                    ]);

                    let nodes = InfoMessage::NodesSearched(self.nodes_searched);
                    
                    let mut info_messages = vec![InfoMessage::PrincipalVariation(
                        pv_algebraic,
                    ), score, nodes];

                    let nodes_per_second = self.search_start.elapsed();
                    if let Ok(duration) = nodes_per_second {
                        let secs = duration.as_secs();
                        if secs > 0 {
                            let nps = self.nodes_searched / secs as usize;
                            info_messages.push(InfoMessage::NodesPerSecond(nps));
                        }
                    }
    
                    // Send it to the output.
                    self
                        .sender
                        .send(OutputMessage::Info(info_messages))
                        .unwrap();
                }
            }
        }

        let tt_item = TranspositionItem {
            key: self.position.hash_key(),
            best_move: None,
            depth: depthleft,
            score: alpha_local,
            node_type: tt_node_type,
            created: SystemTime::now()
        };
        self.transposition_table.store(tt_item);
    
        alpha_local
    }
}



#[cfg(test)]
mod test {
    use std::sync::mpsc;

    use super::*;

    #[test]
    fn test_mate_in_one() {
        let pos: Position = "4k3/8/8/1N5b/2q5/8/8/4K3 b - - 0 1".into();
        let (_tx1, rx1) = mpsc::channel::<InputMessage>();
        let (tx2, _rx2) = mpsc::channel::<OutputMessage>();
        let tree = &mut SearchTree::new(pos, 1, tx2, rx1);
        let mv = tree.search();
        assert_eq!("c4e2", mv.text);

        let pos: Position = "8/7p/8/2p3P1/3k3P/8/p6r/3K4 b - - 0 52".into();
        let (_tx1, rx1) = mpsc::channel::<InputMessage>();
        let (tx2, _rx2) = mpsc::channel::<OutputMessage>();
        // Search to depth 5 to ensure it's not going for longer mates.
        let tree = &mut SearchTree::new(pos, 5, tx2, rx1);
        let mv = tree.search();
        assert_eq!("a2a1q", mv.text);
    }

    #[test]
    fn test_mate_in_two() {
        let pos: Position =
            "r2qkb1r/pp2nppp/3p4/2pNN1B1/2BnP3/3P4/PPP2PPP/R2bK2R w KQkq - 1 0".into();
        let (_tx1, rx1) = mpsc::channel::<InputMessage>();
        let (tx2, _rx2) = mpsc::channel::<OutputMessage>();
        let tree = &mut SearchTree::new(pos, 3, tx2, rx1);
        let mv = tree.search();
        for (i, m) in [(0x43, 0x55), (0x66, 0x55), (0x32, 0x65)].iter().enumerate() {
            assert_eq!(m.0, tree.pv[i].from_index);
            assert_eq!(m.1, tree.pv[i].to_index);
        }
        assert_eq!("d5f6", mv.text);
    }
}
