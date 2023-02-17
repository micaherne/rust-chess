use std::{
    sync::mpsc::{Receiver, Sender},
    thread,
};

use crate::{
    messages::{GoSubcommand, InfoMessage, InputMessage, OutputMessage, ScoreInfo},
    position0x88::{
        evaluate::{evaluate, Score, CHECKMATE_SCORE_MAX},
        movegen::{generate_moves, side_to_move_in_check, Move},
        notation::{make_move, undo_move, LongAlgebraicNotationMove},
        Position,
    },
};

pub type Depth = i16;

type Line = Vec<Move>;

#[derive(Debug)]
pub struct SearchData {
    search_depth: Depth,
    pv: Line,
    sender: Sender<OutputMessage>,
    _receiver: Receiver<InputMessage>,
}

impl SearchData {
    pub fn new(
        depth: Depth,
        sender: Sender<OutputMessage>,
        receiver: Receiver<InputMessage>,
    ) -> Self {
        Self {
            search_depth: depth,
            pv: vec![],
            sender,
            _receiver: receiver,
        }
    }
}

pub fn start_search_thread(
    position: Position,
    _commands: &[GoSubcommand],
    output_sender: Sender<OutputMessage>,
    input_receiver: Receiver<InputMessage>,
) {
    thread::spawn(move || {
        let mut search_data = SearchData::new(4, output_sender, input_receiver);
        let search_position = &mut position.clone();
        search(search_position, &mut search_data);
    });
}

// TODO: This doesn't deal with sending the ponder move back to the engine - it would go direct to
// the uci thread if we sent it here.
pub fn search(position: &mut Position, search_data: &mut SearchData) -> LongAlgebraicNotationMove {
    let mut pline: Line = vec![Move::default(); search_data.search_depth as usize];

    search_ab(
        position,
        Score::MIN / 2,
        Score::MAX / 2, // As -MAX is lower than MIN?
        search_data.search_depth,
        search_data,
        &mut pline,
    );

    let pv_algebraic: Vec<LongAlgebraicNotationMove> = pline.iter().map(|m| m.into()).collect();

    let alg_move = pv_algebraic[0].clone();

    search_data
        .sender
        .send(OutputMessage::BestMove(alg_move.clone(), None))
        .unwrap();

    #[cfg(debug_assertions)]
    println!("Thread exiting");

    return alg_move;
}

fn search_ab(
    position: &mut Position,
    alpha: Score,
    beta: Score,
    depthleft: Depth,
    search_data: &mut SearchData,
    pline: &mut Line,
) -> Score {
    let mut line: Line = vec![];

    // This is a hack to let us modify alpha without changing it to a mutable parameter.
    // TODO: Just sort out the parameters and calls.
    let mut alpha_local = alpha;

    if depthleft == 0 {
        return evaluate(position);
    }

    let moves = generate_moves(position);

    // Test for checkmate / stalemate.
    if moves.len() == 0 {
        if side_to_move_in_check(position) {
            // Bigger for closer mates.
            return  - (depthleft as Score + CHECKMATE_SCORE_MAX);
        } else {
            return 0;
        }
    }

    for mv in moves {
        let undo = make_move(position, mv.from_index, mv.to_index, mv.queening_piece);
        let move_score = -search_ab(
            position,
            -beta,
            -alpha_local,
            depthleft - 1,
            search_data,
            &mut line,
        );
        undo_move(position, undo);

        if move_score >= beta {
            return beta;
        }

        if move_score > alpha_local {
            alpha_local = move_score;

            // If alpha is raised, update the PV.
            pline.clear();
            pline.push(mv);
            pline.extend(&line);

            let pv_algebraic: Vec<LongAlgebraicNotationMove> =
                pline.iter().map(|m| m.into()).collect();

            // Send info output.
            if depthleft == search_data.search_depth {
                // Pull it into the search data.
                search_data.pv = pline.iter().map(|m| m.to_owned()).collect();

                let score = InfoMessage::Score(vec![
                    ScoreInfo::Centipawns(move_score)
                ]);
                // Send it to the output.
                search_data
                    .sender
                    .send(OutputMessage::Info(vec![InfoMessage::PrincipalVariation(
                        pv_algebraic,
                    ), score]))
                    .unwrap();
            }
        }
    }

    alpha_local
}

#[cfg(test)]
mod test {
    use std::sync::mpsc;

    use super::*;

    #[test]
    fn test_mate_in_one() {
        let mut pos: Position = "4k3/8/8/1N5b/2q5/8/8/4K3 b - - 0 1".into();
        let (_tx1, rx1) = mpsc::channel::<InputMessage>();
        let (tx2, _rx2) = mpsc::channel::<OutputMessage>();
        let search_data = &mut SearchData::new(1, tx2, rx1);
        let mv = search(&mut pos, search_data);
        assert_eq!("c4e2", mv.text);

        let mut pos: Position = "8/7p/8/2p3P1/3k3P/8/p6r/3K4 b - - 0 52".into();
        let (_tx1, rx1) = mpsc::channel::<InputMessage>();
        let (tx2, _rx2) = mpsc::channel::<OutputMessage>();
        // Search to depth 5 to ensure it's not going for longer mates.
        let search_data = &mut SearchData::new(5, tx2, rx1);
        let mv = search(&mut pos, search_data);
        assert_eq!("a2a1q", mv.text);
    }

    #[test]
    fn test_mate_in_two() {
        let mut pos: Position =
            "r2qkb1r/pp2nppp/3p4/2pNN1B1/2BnP3/3P4/PPP2PPP/R2bK2R w KQkq - 1 0".into();
        let (_tx1, rx1) = mpsc::channel::<InputMessage>();
        let (tx2, _rx2) = mpsc::channel::<OutputMessage>();
        let search_data = &mut SearchData::new(3, tx2, rx1);
        let mv = search(&mut pos, search_data);
        for (i, m) in [(0x43, 0x55), (0x66, 0x55), (0x32, 0x65)].iter().enumerate() {
            assert_eq!(m.0, search_data.pv[i].from_index);
            assert_eq!(m.1, search_data.pv[i].to_index);
        }
        assert_eq!("d5f6", mv.text);
    }
}
