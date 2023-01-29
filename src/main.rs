pub mod engine;
pub mod fen;
pub mod position0x88;
pub mod uci;

// use std::env;

use crate::{engine::Engine, uci::UciInterface};

fn main() {
    // TODO: Implement args.
    // let args: Vec<String> = env::args().collect();

    let engine = Engine::new();

    let mut uci = UciInterface {
        engine: engine
    };

    uci.run();
}
