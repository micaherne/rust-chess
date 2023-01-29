use std::{
    collections::HashMap,
    sync::mpsc::{Receiver, Sender},
};

use crate::{
    messages::{InputMessage, OutputMessage},
    position0x88::{
        notation::{set_from_fen, set_startpos},
        Position,
    },
};

pub struct Engine {
    pub position: Position,
    options: EngineOptions,
    initialised: bool,
    receiver: Receiver<InputMessage>,
    sender: Sender<OutputMessage>,
}

struct EngineOptions {
    debug: bool,
    options: HashMap<String, String>,
}

impl Engine {
    pub fn new(receiver: Receiver<InputMessage>, sender: Sender<OutputMessage>) -> Engine {
        return Engine {
            position: Position::default(),
            options: EngineOptions {
                debug: false,
                options: HashMap::new(),
            },
            initialised: false,
            receiver,
            sender,
        };
    }

    pub fn init(&mut self) {
        println!("Initialising!");
        self.initialised = true;
    }

    pub fn set_option_debug(&mut self, value: bool) {
        self.options.debug = value;
    }

    pub fn set_option(&mut self, id: String, value: String) {
        self.options.options.insert(id, value);
    }

    pub fn new_game(&mut self) {
        // TODO: Empty caches etc.
    }

    pub fn listen(&mut self) {
        loop {
            // If anything goes wrong with the messaging we just quit.
            let message = self.receiver.recv().unwrap_or(InputMessage::Quit);

            match message {
                InputMessage::Quit => {
                    break;
                }
                InputMessage::SetStartPosition => set_startpos(&mut self.position),
                InputMessage::SetPositionFromFen(fen) => {
                    set_from_fen(&mut self.position, &fen).unwrap()
                }
                InputMessage::MakeMoves(moves) => println!("Making {} moves", moves.len()),
                InputMessage::GetAvailableOptions => self
                    .sender
                    .send(OutputMessage::AvailableOptions(vec![]))
                    .unwrap(),
                InputMessage::IsReady => self.sender.send(OutputMessage::Ready).unwrap(),
                InputMessage::SetDebug(value) => self.set_option_debug(value),
                InputMessage::NewGame => {
                    // TODO: Clear caches and stuff.
                }
            }
        }
        self.sender.send(OutputMessage::Quitting).unwrap();
    }
}
