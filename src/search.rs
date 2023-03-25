use std::{
    sync::mpsc::{Receiver, Sender},
    thread,
    time::SystemTime,
};

use chess_uci::messages::{
    GoSubcommand, InfoMessage, InputMessage, LongAlgebraicNotationMove, OutputMessage, ScoreInfo,
};

use crate::{
    position::Evaluate,
    position0x88::{
        evaluate::{Score, CHECKMATE_SCORE_MAX},
        make_moves::MakeMoves,
        movegen::{GenerateMoves, Move},
        movegen_simple::side_to_move_in_check,
        Position0x88, WHITE,
    },
    transposition::{NodeType, TranspositionItem, TranspositionTable},
};

#[cfg(debug_assertions)]
use crate::position0x88::notation::to_fen;

pub type Depth = i16;

type Line = Vec<Move>;

const TRANSPOSITION_TABLE_SIZE: usize = 1_000_000;

#[derive(Debug)]
pub struct SearchTree {
    position: Position0x88,
    search_depth: Depth,
    transposition_table: TranspositionTable,
    pv: Line,
    nodes_searched: usize,
    time_allowed: usize, // in milliseconds - zero is infinity
    timeout: bool,       // have we run out of time and need to return immediately?
    search_start: SystemTime,
    sender: Sender<OutputMessage>,
    _receiver: Receiver<InputMessage>,
}

impl SearchTree {
    pub fn new(
        position: Position0x88,
        depth: Depth,
        time_allowed: usize,
        sender: Sender<OutputMessage>,
        receiver: Receiver<InputMessage>,
    ) -> Self {
        Self {
            position,
            search_depth: depth,
            transposition_table: TranspositionTable::new(TRANSPOSITION_TABLE_SIZE),
            pv: vec![],
            nodes_searched: 0,
            time_allowed,
            timeout: false,
            search_start: SystemTime::now(),
            sender,
            _receiver: receiver,
        }
    }

    /// Create the search tree and start the thread to search it.
    pub fn start_search_thread(
        position: Position0x88,
        commands: &[GoSubcommand],
        output_sender: Sender<OutputMessage>,
        input_receiver: Receiver<InputMessage>,
    ) {
        let search_time_allowed = SearchTree::calculate_time_allowed(&position, &commands);

        let mut depth = 5;

        for cmd in commands {
            match cmd {
                GoSubcommand::Depth(x) => depth = *x as Depth,
                _ => {}
            }
        }
        let mut tree = SearchTree::new(
            position,
            depth,
            search_time_allowed,
            output_sender,
            input_receiver,
        );

        thread::spawn(move || {
            tree.search();
        });
    }

    pub fn calculate_time_allowed(position: &Position0x88, commands: &[GoSubcommand]) -> usize {
        let mut moves_to_go = 0;
        let mut time_left = 0;
        let mut increment = 0;
        let wtm = position.get_side_to_move() == WHITE;

        for command in commands {
            match command {
                GoSubcommand::WTime(x) => {
                    if wtm {
                        time_left = x.clone()
                    }
                }
                GoSubcommand::BTime(x) => {
                    if !wtm {
                        time_left = x.clone()
                    }
                }
                GoSubcommand::WInc(x) => {
                    if wtm {
                        increment = x.clone()
                    }
                }
                GoSubcommand::BInc(x) => {
                    if !wtm {
                        increment = x.clone()
                    }
                }
                GoSubcommand::MovesToGo(x) => moves_to_go = x.clone(),
                GoSubcommand::Infinite => return 0,
                _ => {}
            }
        }

        if moves_to_go == 0 {
            // It's sudden death.
            // TODO: this is stupid - find a better way to do it.
            (time_left / 40) as usize
        } else {
            (time_left / moves_to_go + increment) as usize
        }
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

        self.sender
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

        if self.nodes_searched % 20000 == 0 {
            let res = self._receiver.try_recv();
            if res.is_ok() {
                let message = res.unwrap();
                match message {
                    InputMessage::Stop(_) => {
                        // TODO: This doesn't work any more. Get the PV / best move
                        // and send it before exiting.
                        self.timeout = true;
                        return 0;
                    }
                    _ => {
                        #[cfg(debug_assertions)]
                        println!("Unexpected input message!");
                    }
                }
            }
        }

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
                #[cfg(debug_assertions)]
                {
                    let to_fen = to_fen(&self.position);
                    if from_tt_val.fen != to_fen {
                        // Check it's not just the move number that's different.
                        let frm: Vec<&str> =
                            from_tt_val.fen.split_ascii_whitespace().take(5).collect();
                        let to: Vec<&str> = to_fen.split_ascii_whitespace().take(5).collect();
                        if frm.join(" ") != to.join(" ") {
                            println!("Hash collision: {} {}", from_tt_val.fen, to_fen);
                        }
                    }
                }

                match from_tt_val.node_type {
                    NodeType::PV => return from_tt_val.score,
                    NodeType::All => {
                        if from_tt_val.score <= alpha {
                            return alpha;
                        }
                    }
                    NodeType::Cut => {
                        if from_tt_val.score > beta {
                            return beta;
                        }
                    }
                }
            }
        }

        if depthleft == 0 {
            let eval = self.position.evaluate(); // self.quiesce(alpha_local, beta);
            let tt_item = TranspositionItem {
                key: self.position.hash_key(),
                best_move: None,
                depth: depthleft,
                score: eval,
                node_type: NodeType::PV,
                created: SystemTime::now(),
                #[cfg(debug_assertions)]
                fen: to_fen(&self.position),
            };
            self.transposition_table.store(tt_item);
            return eval;
        }

        let moves = &self.position.generate_moves();

        // Test for checkmate / stalemate.
        if moves.len() == 0 {
            if side_to_move_in_check(&self.position) {
                // Bigger for closer mates.
                return -(depthleft as Score + CHECKMATE_SCORE_MAX);
            } else {
                return 0;
            }
        }

        for mv in moves {
            let undo = self
                .position
                .make_move(mv.from_index, mv.to_index, mv.queening_piece);
            let move_score = -self.search_ab(-beta, -alpha_local, depthleft - 1, &mut line);

            self.position.undo_move(undo);

            if move_score >= beta {
                let tt_item = TranspositionItem {
                    key: self.position.hash_key(),
                    best_move: None,
                    depth: depthleft,
                    score: beta,
                    node_type: NodeType::Cut,
                    created: SystemTime::now(),
                    #[cfg(debug_assertions)]
                    fen: to_fen(&self.position),
                };
                self.transposition_table.store(tt_item);

                return beta;
            }

            if move_score > alpha_local {
                alpha_local = move_score;

                tt_node_type = NodeType::PV;

                // If alpha is raised, update the PV.
                pline.clear();
                pline.push(*mv);
                pline.extend(&line);

                let pv_algebraic: Vec<LongAlgebraicNotationMove> =
                    pline.iter().map(|m| m.into()).collect();

                // Send info output.
                if depthleft == self.search_depth {
                    // Pull it into the search data.
                    self.pv = pline.iter().map(|m| m.to_owned()).collect();

                    // Show the score from the engine's point of view.
                    let score_for_me = match depthleft & 1 {
                        0 => -move_score,
                        1 => move_score,
                        _ => {
                            #[cfg(debug_assertions)]
                            println!("Unexpected value for depth mod 2");
                            0
                        }
                    };

                    let score = InfoMessage::Score(vec![ScoreInfo::Centipawns(score_for_me)]);

                    let nodes = InfoMessage::NodesSearched(self.nodes_searched);

                    let mut info_messages =
                        vec![InfoMessage::PrincipalVariation(pv_algebraic), score, nodes];

                    let time_elapsed = self.search_start.elapsed();
                    if let Ok(duration) = time_elapsed {
                        let millis = duration.as_millis();

                        // Bail out if we have no more time.
                        if self.time_allowed != 0 && millis > (self.time_allowed as u128) {
                            #[cfg(debug_assertions)]
                            println!("Setting timeout!!!!!!!!");
                            self.timeout = true;
                            return 0;
                        }

                        info_messages.push(InfoMessage::TimeSearched(millis as usize));
                        let secs = duration.as_secs();
                        if secs > 0 {
                            let nps = self.nodes_searched / secs as usize;
                            info_messages.push(InfoMessage::NodesPerSecond(nps));
                        }

                        info_messages.push(InfoMessage::Depth(self.search_depth as usize));
                    }

                    // Send it to the output.
                    self.sender
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
            created: SystemTime::now(),
            #[cfg(debug_assertions)]
            fen: to_fen(&self.position),
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
        let pos: Position0x88 = "4k3/8/8/1N5b/2q5/8/8/4K3 b - - 0 1".into();
        let (_tx1, rx1) = mpsc::channel::<InputMessage>();
        let (tx2, _rx2) = mpsc::channel::<OutputMessage>();
        let tree = &mut SearchTree::new(pos, 1, 0, tx2, rx1);
        let mv = tree.search();
        assert_eq!("c4e2", mv.text);

        let pos: Position0x88 = "8/7p/8/2p3P1/3k3P/8/p6r/3K4 b - - 0 52".into();
        let (_tx1, rx1) = mpsc::channel::<InputMessage>();
        let (tx2, _rx2) = mpsc::channel::<OutputMessage>();
        // Search to depth 5 to ensure it's not going for longer mates.
        let tree = &mut SearchTree::new(pos, 5, 0, tx2, rx1);
        let mv = tree.search();
        assert_eq!("a2a1q", mv.text);
    }

    #[test]
    fn test_mate_in_two() {
        let pos: Position0x88 =
            "r2qkb1r/pp2nppp/3p4/2pNN1B1/2BnP3/3P4/PPP2PPP/R2bK2R w KQkq - 1 0".into();
        let (_tx1, rx1) = mpsc::channel::<InputMessage>();
        let (tx2, _rx2) = mpsc::channel::<OutputMessage>();
        let tree = &mut SearchTree::new(pos, 3, 0, tx2, rx1);
        let mv = tree.search();
        for (i, m) in [(0x43, 0x55), (0x66, 0x55), (0x32, 0x65)]
            .iter()
            .enumerate()
        {
            assert_eq!(m.0, tree.pv[i].from_index);
            assert_eq!(m.1, tree.pv[i].to_index);
        }
        assert_eq!("d5f6", mv.text);
    }

    #[test]
    fn test_stupid_move() {
        let pos: Position0x88 = "r2r1k2/ppp3pp/2p1p3/6P1/1b3B2/2N2P2/PP5P/2KRR3 b - - 0 20".into();
        let (_tx1, rx1) = mpsc::channel::<InputMessage>();
        let (tx2, _rx2) = mpsc::channel::<OutputMessage>();
        let tree = &mut SearchTree::new(pos, 5, 0, tx2, rx1);
        let mv = tree.search();
        println!("{:#?}", tree.pv);
        assert_ne!("d8d7", mv.text);
    }

    /*
    #[test]
    fn test_search() {
        return;
        let cwd = env::current_dir().unwrap();
        let root = cwd.ancestors().next().unwrap();
        let path = root.join("tests/wac.epd");
        let perft_contents = fs::read_to_string(path).unwrap();
        let perft_lines = perft_contents.split("\n");
        for line in perft_lines {
            if line.trim() == "" {
                break;
            }
            let line_parts: Vec<&str> = line.split_whitespace().collect();
            let fen = line_parts[0..4].join(" ");
            let rest = line_parts[4..].join(" ");
            let mut operations = rest.split(";");
            let mut best_moves = operations.next().unwrap().split(" ");
            if best_moves.next().unwrap() != "bm" {
                println!("Invalid");
                continue;
            }
            let best_move = best_moves.next().unwrap();
            if best_moves.next().is_some() {
                println!("Ignoring multiple best moves");
                continue;
            }

            let re = Regex::new("([RKBQK]?)x?([a-h][1-8])\\+?").unwrap();

            let p = re.captures(best_move).unwrap();

            let to_square = p.get(2).unwrap().as_str();
            let piece_type = p.get(1).unwrap().as_str();

            let pos: Position = fen.as_str().into();

            let (sender, _t) = mpsc::channel::<OutputMessage>();
            let (_x, receiver) = mpsc::channel::<InputMessage>();

            let mut tree = SearchTree::new(pos, 7, 1000000, sender, receiver);

            tree.search();

            let best_move_to = square_index_to_str(tree.pv[0].to_index);

            println!("{} {} {}", best_move_to, to_square, piece_type);
            if best_move_to != to_square {
                println!("{}", fen);
                break;
            }
        }
    }*/
}
