pub mod board;
pub mod engine;
pub mod fen;
pub mod uci;

use std::env;

use crate::{engine::Engine, uci::UciInterface};

fn main() {
    let args: Vec<String> = env::args().collect();

    let engine = Engine::new();

    let mut uci = UciInterface {
        engine: engine
    };

    uci.run();
}
