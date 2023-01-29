use crate::{position0x88::Position};

pub struct Engine {
    pub board: Position,
    options: EngineOptions,
    initialised: bool,
}

struct EngineOptions {
    debug: bool,
}

impl Engine {
    pub fn new() -> Engine {
        return Engine {
            board: Position::default(), // Board::default(),
            options: EngineOptions { debug: false },
            initialised: false,
        };
    }

    pub fn init(&mut self) {
        println!("Initialising!");
        self.initialised = true;
    }
}
