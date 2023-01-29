pub mod engine;
pub mod fen;
pub mod messages;
pub mod position0x88;
pub mod uci;

// use std::env;

use std::{thread, sync::mpsc};

use messages::{InputMessage, OutputMessage};
use uci::{UciInputListener, UciOutputListener};

use crate::{engine::Engine};

fn main() {
    // TODO: Implement args.
    // let args: Vec<String> = env::args().collect();

    let (input_sender, input_receiver) = mpsc::channel::<InputMessage>();
    let (output_sender, output_receiver) = mpsc::channel::<OutputMessage>();


    let mut engine = Engine::new(input_receiver, output_sender);
    let engine_handle = thread::spawn(move || {
        engine.listen();
    });

    let uci_input = UciInputListener {
        sender: input_sender
    };

    let uci_output = UciOutputListener {
        receiver: output_receiver
    };

    let h = thread::spawn(move || {
        uci_input.listen();
    });

    let output_handle = thread::spawn(move || {
        uci_output.listen();
    });
    
    engine_handle.join().unwrap();
    h.join().unwrap();
    output_handle.join().unwrap();

    
}
