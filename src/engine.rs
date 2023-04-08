use std::{
    collections::HashMap,
    sync::mpsc::{self, Receiver, Sender},
};

use chess_uci::messages::{InputMessage, OutputMessage};

use crate::{
    fen::STARTPOS_FEN,
    position64::{
        moves::{MakeMove, Move, MoveUndo},
        Position64,
    },
    search::SearchTree,
};

pub struct Engine {
    pub position: Position64,
    options: EngineOptions,
    initialised: bool,
    receiver: Receiver<InputMessage>,
    sender: Sender<OutputMessage>,
    undo_stack: Vec<MoveUndo>, // the moves made to get to the position to be searched
}

struct EngineOptions {
    debug: bool,
    options: HashMap<String, String>,
}

impl Engine {
    pub fn new(receiver: Receiver<InputMessage>, sender: Sender<OutputMessage>) -> Engine {
        return Engine {
            position: Position64::default(),
            options: EngineOptions {
                debug: false,
                options: HashMap::new(),
            },
            initialised: false,
            receiver,
            sender,
            undo_stack: vec![],
        };
    }

    pub fn init(&mut self) {
        #[cfg(debug_assertions)]
        println!("Initialising!");

        // TODO: Do initialising stuff.

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
        // Sender for input messages into the search thread.
        let mut input_sender: Option<Sender<InputMessage>> = None;
        loop {
            // If anything goes wrong with the messaging we just quit.
            let message = self.receiver.recv().unwrap_or(InputMessage::Quit);

            match message {
                InputMessage::Quit => {
                    break;
                }
                InputMessage::SetStartPosition => self.position = STARTPOS_FEN.parse().unwrap(),
                InputMessage::SetPositionFromFen(fen) => {
                    self.position = fen.parse().unwrap();
                }
                InputMessage::MakeMoves(moves) => {
                    let the_moves: Vec<Move> = moves
                        .into_iter()
                        .map(|x| {
                            let v: Move = x.try_into().expect("invalid algebraic move");
                            v
                        })
                        .collect();
                    self.undo_stack = self.position.make_moves(&the_moves)
                }
                InputMessage::GetAvailableOptions => self
                    .sender
                    .send(OutputMessage::AvailableOptions(vec![]))
                    .unwrap(),
                InputMessage::IsReady => {
                    if self.initialised == false {
                        self.init();
                    }
                    self.sender.send(OutputMessage::Ready).unwrap()
                }
                InputMessage::SetDebug(value) => self.set_option_debug(value),
                InputMessage::NewGame => {
                    // TODO: Clear caches and stuff.
                }
                InputMessage::Go(subcommands) => {
                    // Stop any existing thread.
                    if input_sender.is_some() {
                        let sender = input_sender.take().unwrap();
                        let send_result = sender.send(InputMessage::Stop(false));
                        if send_result.is_ok() {
                            // Do what?
                        }
                    }

                    // For output back to the UCI thread.
                    let (tx, rx) = mpsc::channel();
                    input_sender = Some(tx);
                    let output_sender = self.sender.clone();
                    Some(SearchTree::start_search_thread(
                        self.position,
                        &subcommands,
                        output_sender,
                        rx,
                    ));
                }
                InputMessage::Stop(to_send) => {
                    if input_sender.is_some() {
                        let sender = input_sender.take().unwrap();
                        let res = sender.send(InputMessage::Stop(to_send));
                        if !res.is_ok() {
                            #[cfg(debug_assertions)]
                            println!("Error sending stop message");
                        }
                    }
                }
                InputMessage::SendId => {
                    self.sender
                        .send(OutputMessage::Id(vec![
                            ("name".to_owned(), "Dexy".to_owned()),
                            ("author".to_owned(), "Michael Aherne".to_owned()),
                        ]))
                        .unwrap();
                    self.sender.send(OutputMessage::UciOk).unwrap();
                }
                InputMessage::SetOption(_, _) => {
                    if self.initialised == false {
                        self.init();
                    }
                    todo!()
                }
                InputMessage::PonderHit => todo!(),
            }
        }
        self.sender.send(OutputMessage::Quitting).unwrap();
    }
}
