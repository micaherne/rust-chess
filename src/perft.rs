use std::{
    collections::{HashMap, VecDeque},
    io::{BufRead, BufReader, Write},
    process::{Child, ChildStdin, ChildStdout, Command, Stdio}, u8,
};

use crate::position0x88::{
    movegen::generate_moves,
    notation::{
        make_move, make_moves, set_from_fen, to_fen, undo_move,
        LongAlgebraicNotationMove,
    },
    Position,
};

#[cfg(debug_assertions)]
use crate::position0x88::notation::square_index_to_str;

pub fn perft(position: &mut Position, depth: u16) -> usize {
    if depth == 0 {
        return 1;
    }

    // println!("Depth: {}", depth);

    assert!(depth > 0);

    let moves = generate_moves(position);
    let mut nodes = 0;

    if depth == 1 {
        return moves.len();
    }

    for m in moves {
        let undo = make_move(position, m.from_index, m.to_index, m.queening_piece);
        nodes += perft(position, depth - 1);
        undo_move(position, undo);
    }

    nodes
}

pub fn divide(position: &mut Position, depth: u16) -> DivideResults {
    let mut result = DivideResults::default();

    let moves = generate_moves(&position);

    let mut total = 0;

    for m in moves {

        #[cfg(debug_assertions)]
        let before_fen = to_fen(position);

        #[cfg(debug_assertions)]
        let before_hash = position.hash_key();

        let undo = make_move(position, m.from_index, m.to_index, m.queening_piece);
        let nodes = perft(position, depth - 1);
        total += nodes;
        undo_move(position, undo);

        #[cfg(debug_assertions)]
        {
            let after_fen = to_fen(position);

            if before_fen != after_fen {
                panic!("Move: {}{}, before: {}, after: {}",
                    square_index_to_str(m.from_index),
                    square_index_to_str(m.to_index),
                    before_fen, after_fen
                );
            }

            let after_hash = position.hash_key();
            if before_hash != after_hash {
                panic!("Move: {}{}, before: {}, after: {}",
                    square_index_to_str(m.from_index),
                    square_index_to_str(m.to_index),
                    before_hash, after_hash
                );
            }
        }

        result.move_counts.insert(m.to_string(), nodes);
    }

    result.total = total;

    result
}

pub fn run_perft(args: VecDeque<String>) {
    println!("PERFT!!!!! {:#?}", args);
}

pub fn run_perft_compare(args: &mut VecDeque<String>) {
    let mut stockfish = StockfishConnector::new(
        "C:\\Users\\michael\\dev\\rust\\stockfish-windows-2022-x86-64-modern.exe",
    );
    // let fen = "r3kbnr/p7/1p1B1q2/3b1p1P/PpPp4/2Q4N/3PPP1P/RN2KB1R b KQkq - 0 1";
    // let fen = "8/8/5k2/p1q5/PP1rp1P1/3P1N1B/2RKp2r/6N1 b - -";
    // let fen = "rb6/5b2/1p2r3/p1k1P3/PpP1p3/2R4P/3P4/1N1K2R1 w - -";
    // let fen = "r3kbnr/2qn2p1/8/pppBpp1P/3P1Pb1/P1P1P3/1P2Q2P/RNB1K1NR w KQkq - 0 1";
    // let fen = "rn3b1r/1bqpp1k1/p7/2p2p1p/P2P4/2N1P1P1/1pK1NPP1/R3QB1R b - - 0 1";
    // let fen = "rb6/5b2/1pr5/p1k1P3/PpP1p3/5R1P/3P2R1/1N1K4 b - - 0 1";
    // let fen = "2b1kbn1/r1pqp3/n2p3p/3P1pp1/ppP3P1/PPB1P2P/Q4P2/RN2KBNR b KQ - 0 1";
    // let fen = "rB5r/pp4k1/5n2/q3p2p/Pb3pp1/1P1P3N/R2QPPP1/1N2KB1R w K - 0 1";

    if args.len() != 2 {
        println!("Usage perftcompare [fen] [depth]");
        return;
    }

    let arg1 = args.pop_front().unwrap();
    let fen: &str = arg1.as_str();

    let arg2 = args.pop_front().unwrap();
    let parse_result = arg2.parse::<u8>();
    if let Err(_) = parse_result {
        print!("Invalid depth");
        return;
    }

    let start_depth: u8 = parse_result.unwrap();

    let mut moves: Vec<String> = vec![];

    for depth in (1..start_depth).rev() {
        let diff = get_diff(&mut stockfish, fen, depth, &mut moves);
        let mut biggest_diff_move: String = "".to_string();
        let max_diff = 0;

        for (mv, counts) in diff.different_counts {
            if counts.1.abs_diff(counts.0) > max_diff {
                biggest_diff_move = mv;
            }
        }

        if diff.missing_moves.len() > 0 {
            println!("Missing moves: {}", diff.missing_moves.join(", "));
        }

        if biggest_diff_move.len() == 0 {
            println!("No different counts at depth {}", depth);
            return;
        }

        println!("Depth: {}, Move: {}", depth, biggest_diff_move);

        moves.push(biggest_diff_move);
    }

    stockfish.quit();

    // println!("Moves: {:#?}", moves);
}

fn get_diff(
    stockfish: &mut StockfishConnector,
    fen: &str,
    depth: u8,
    moves: &Vec<String>,
) -> DivideDiff {
    // Get perft at a certain depth.
    let their_perft = stockfish.get_perft(fen, moves, depth);
    let mut pos = Position::default();
    set_from_fen(&mut pos, fen).unwrap();

    let l: Vec<LongAlgebraicNotationMove> = moves
        .iter()
        .map(|m| LongAlgebraicNotationMove {
            text: m.to_string(),
        })
        .collect();

    make_moves(&mut pos, &l);

    let f = to_fen(&pos);
    println!("{}", f);

    let our_perft = divide(&mut pos, depth as u16);

    let diff = diff_divides(&our_perft, &their_perft);

    diff
}

#[derive(Default, Debug)]
pub struct DivideResults {
    move_counts: HashMap<String, usize>,
    total: usize,
}

#[derive(Debug, Default, Clone)]
struct DivideDiff {
    missing_moves: Vec<String>,
    different_counts: HashMap<String, (usize, usize)>,
}

struct StockfishConnector {
    child: Child,
    stdin: ChildStdin,
    stdout: ChildStdout,
}

impl StockfishConnector {
    fn new(path: &str) -> Self {
        let mut command = Command::new(path);
        let mut child = command
            .stdout(Stdio::piped())
            .stdin(Stdio::piped())
            .spawn()
            .unwrap();
        let stdout = child.stdout.take().unwrap();
        let stdin = child.stdin.take().unwrap();
        StockfishConnector {
            child: child,
            stdin: stdin,
            stdout: stdout,
        }
    }

    fn quit(&mut self) {
        let res = writeln!(self.stdin, "quit");
        match res {
            Ok(()) => {
                self.child.wait().unwrap();
            }
            Err(_) => self.child.kill().unwrap(),
        }
    }

    fn get_perft(&mut self, fen: &str, moves: &Vec<String>, depth: u8) -> DivideResults {
        let mut result = DivideResults::default();

        if moves.len() > 0 {
            let msg = format!("position fen {} moves {}", fen, moves.join(" "));
            println!("To Stockfish: {}", msg);
            writeln!(self.stdin, "{}", msg).expect("Write failed");
        } else {
            let msg = format!("position fen {}", fen);
            writeln!(self.stdin, "{}", msg).expect("Write failed");
        }

        writeln!(self.stdin, "go perft {}", depth).expect("Write failed");

        let mut f = BufReader::new(&mut self.stdout);

        loop {
            let mut buf = String::new();
            match f.read_line(&mut buf) {
                Ok(_) => {
                    if buf.contains(":") {
                        let mut parts = buf.split(":");
                        let mv = parts.next().unwrap();
                        let nodes: usize = parts.next().unwrap().trim().parse().unwrap();

                        if buf.starts_with("Nodes searched:") {
                            result.total = nodes;
                            break;
                        } else {
                            result.move_counts.insert(mv.to_owned(), nodes);
                        }
                    }
                }
                Err(e) => println!("an error!: {:?}", e),
            }
        }

        result
    }
}

fn diff_divides(ours: &DivideResults, theirs: &DivideResults) -> DivideDiff {
    let mut result = DivideDiff::default();
    for (m, _) in &theirs.move_counts {
        let mv = m.to_owned();
        if !ours.move_counts.contains_key(&mv) {
            result.missing_moves.push(m.to_string());
        } else {
            let o = *ours.move_counts.get(&mv).unwrap();
            let t = *theirs.move_counts.get(&mv).unwrap();
            if o != t {
                result.different_counts.insert(mv, (t, o));
            }
        }
    }
    result
}

pub fn run_divide(args: VecDeque<String>) {
    if args.len() < 1 {
        println!("Usage divide [fen] [depth=1]");
    }
    let fen = &args[0];

    let depth = &args[1];
    let depth_int: u16 = depth.parse().unwrap_or(1);

    let mut position = Position::default();
    let setup_result = set_from_fen(&mut position, fen);

    if setup_result.is_err() {
        println!("Invalid FEN");
        return;
    }

    let moves = generate_moves(&position);

    let mut total = 0;

    for m in moves {
        let undo = make_move(&mut position, m.from_index, m.to_index, m.queening_piece);
        let nodes = perft(&mut position, depth_int - 1);
        total += nodes;
        undo_move(&mut position, undo);
        println!("{} {}", m.to_string(), nodes);
    }

    println!("Total nodes: {}", total);
}
