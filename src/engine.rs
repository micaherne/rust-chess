use crate::board::Board;

pub struct Engine {
    pub board: Board,
    options: EngineOptions,
    initialised: bool,
}

struct EngineOptions {
    debug: bool,
}

impl Engine {
    pub fn new() -> Engine {
        return Engine {
            board: Board::default(),
            options: EngineOptions { debug: false },
            initialised: false,
        };
    }

    pub fn init(&mut self) {
        println!("Initialising!");
        self.initialised = true;
    }
}
