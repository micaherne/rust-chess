pub mod bitboards;
pub mod engine;
pub mod fen;
pub mod magic;
pub mod perft;
pub mod position;
pub mod position64;
pub mod search;
pub mod transposition;
pub mod zobrist;

use std::{collections::VecDeque, env, sync::mpsc, thread};

use perft::{run_divide, run_perft, run_perft_compare};

use crate::engine::Engine;

fn main() {
    let mut args: VecDeque<String> = env::args().collect();

    let _executable = args.pop_front();

    let command_str = args.pop_front().unwrap_or("uci".to_string());
    let command = command_str.as_str();

    match command {
        "uci" => run_uci(),
        "perft" => run_perft(args),
        "perftcompare" => run_perft_compare(&mut args),
        "divide" => run_divide(args).unwrap(),
        _ => println!("Invalid command {}", command),
    }
}

fn run_uci() {
    let (output_sender, output_receiver) = mpsc::channel::<chess_uci::messages::OutputMessage>();

    let input_receiver = chess_uci::listen(output_receiver);

    let mut engine = Engine::new(input_receiver, output_sender);

    let engine_handle = thread::spawn(move || {
        engine.listen();
    });

    engine_handle.join().unwrap();
}
