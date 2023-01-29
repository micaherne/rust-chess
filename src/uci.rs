use crate::{
    engine::Engine,
    messages::{InputMessage, OutputMessage, AvailableOption},
    position0x88::notation::{make_moves, set_from_fen, set_startpos, LongAlgebraicNotationMove},
};

use std::{
    collections::VecDeque,
    io,
    sync::mpsc::{Receiver, Sender},
};

pub struct UciInputListener {
    pub sender: Sender<InputMessage>,
}

pub struct UciOutputListener {
    pub receiver: Receiver<OutputMessage>,
}

impl UciOutputListener {
    pub fn listen(self) {
        loop {
            let message = self.receiver.recv();
            match message {
                Err(_) => println!("Error"),
                Ok(OutputMessage::Quitting) => break,
                Ok(m) => self.process_message(m)
            }
        }
    }

    fn process_message(&self, message: OutputMessage) {
        match message {
            OutputMessage::AvailableOptions(m) => self.process_available_options(m),
            OutputMessage::Ready => println!("readyok"),
            OutputMessage::Quitting => {}
        }
    }

    fn process_available_options(&self, options: Vec<AvailableOption>) {
        for option in options {
            
        }
    }
}

impl UciInputListener {
    pub fn listen(self) {
        loop {
            let mut input = String::new();

            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");

            let input = input.trim();

            if input == "" {
                continue;
            }

            if input == "quit" {
                self.sender.send(InputMessage::Quit).unwrap();
                break;
            }

            self.process_input(input);
        }
    }

    fn process_input(&self, command: &str) {
        let valid_commands = [
            "uci",
            "debug",
            "isready",
            "setoption",
            "register",
            "ucinewgame",
            "position",
            "go",
            "stop",
            "ponderhit",
            "quit",
        ];

        let mut tokens: VecDeque<&str> = command.trim().split_whitespace().collect();

        // From UCI spec, unknown words should be ignored and the rest of the line processed.
        loop {
            let keyword_result = tokens.pop_front();
            match keyword_result {
                Some(token) => {
                    if valid_commands.contains(&token) {
                        self.run_command(token, &mut tokens);
                        break;
                    }
                }
                None => return,
            }
        }
    }

    fn run_command(&self, keyword: &str, args: &mut VecDeque<&str>) {
        match keyword {
            "uci" => self.process_uci(),
            "debug" => self.process_debug(args),
            "isready" => self.process_isready(),
            "setoption" => self.process_setoption(args),
            "register" => self.process_register(args),
            "ucinewgame" => self.process_ucinewgame(),
            "position" => self.process_position(args),
            "go" => self.process_go(args),
            "stop" => self.process_stop(),
            "ponderhit" => self.process_ponderhit(),
            "quit" => {} // Quit followed by any other text should just be ignored.
            _ => panic!("Unknown keyword"),
        }
    }

    fn process_uci(&self) {
        println!("id name Unidexter-R");
        println!("id author Michael Aherne");
        // TODO: send options.
        println!("uciok")
    }

    fn process_debug(&self, args: &mut VecDeque<&str>) {
        match args.pop_front() {
            Some(val) => match val.trim() {
                "on" => self.sender.send(InputMessage::SetDebug(true)).unwrap(),
                "off" => self.sender.send(InputMessage::SetDebug(false)).unwrap(),
                _ => println!("Invalid value"),
            },
            None => println!("Value required"),
        }
    }

    fn process_isready(&self) {
        self.sender.send(InputMessage::IsReady).unwrap();
    }

    fn process_setoption(&self, args: &mut VecDeque<&str>) {
        todo!()
    }

    fn process_register(&self, args: &mut VecDeque<&str>) {
        todo!()
    }

    fn process_ucinewgame(&self) {
        self.sender.send(InputMessage::NewGame);
    }

    fn process_position(&self, args: &mut VecDeque<&str>) {
        let pos_type_opt = args.pop_front();

        if pos_type_opt == None {
            return;
        }

        let pos_type = pos_type_opt.unwrap();

        if pos_type == "startpos" {
            self.sender.send(InputMessage::SetStartPosition).unwrap();
        } else if pos_type == "fen" {
            let mut fen_string = String::new();

            while args.front() != None && "moves" != *args.front().unwrap() {
                let arg = args.pop_front().unwrap();

                fen_string.push_str(arg);
                fen_string.push_str(" ");
            }

            self.sender
                .send(InputMessage::SetPositionFromFen(fen_string))
                .unwrap();
        } else {
            return;
        }

        if args.len() == 0 {
            return;
        }

        if "moves" != args.pop_front().unwrap() {
            // Something is wrong here.
            return;
        }

        let mut moves: Vec<LongAlgebraicNotationMove> = Vec::new();
        for move_text in args {
            let m = LongAlgebraicNotationMove::from_text(move_text.to_string());
            match m {
                Ok(m) => moves.push(m),
                Err(_e) => println!("Invalid move {0}", move_text),
            }
        }

        self.sender.send(InputMessage::MakeMoves(moves)).unwrap();
    }

    fn process_go(&self, args: &mut VecDeque<&str>) {
        todo!()
    }

    fn process_stop(&self) {
        todo!()
    }

    fn process_ponderhit(&self) {
        todo!()
    }
}

pub struct UciInterface {
    pub sender: Sender<InputMessage>,
    pub engine: Engine,
}

impl UciInterface {
    fn process_command(&mut self, command: &str) {
        let valid_commands = [
            "uci",
            "debug",
            "isready",
            "setoption",
            "register",
            "ucinewgame",
            "position",
            "go",
            "stop",
            "ponderhit",
            "quit",
        ];

        let mut tokens: VecDeque<&str> = command.trim().split_whitespace().collect();

        // From UCI spec, unknown words should be ignored and the rest of the line processed.
        loop {
            let keyword_result = tokens.pop_front();
            match keyword_result {
                Some(token) => {
                    if valid_commands.contains(&token) {
                        self.run_command(token, &mut tokens);
                        break;
                    }
                }
                None => return,
            }
        }
    }

    fn run_command(&mut self, keyword: &str, args: &mut VecDeque<&str>) {
        match keyword {
            "uci" => self.process_uci(),
            "debug" => self.process_debug(args),
            "isready" => self.process_isready(),
            "setoption" => self.process_setoption(args),
            "register" => self.process_register(args),
            "ucinewgame" => self.process_ucinewgame(),
            "position" => self.process_position(args),
            "go" => self.process_go(args),
            "stop" => self.process_stop(),
            "ponderhit" => self.process_ponderhit(),
            "quit" => {} // Quit followed by any other text should just be ignored.
            _ => panic!("Unknown keyword"),
        }
    }

    fn process_debug(&mut self, args: &mut VecDeque<&str>) {
        match args.pop_front() {
            Some(val) => match val.trim() {
                "on" => self.engine.set_option_debug(true),
                "off" => self.engine.set_option_debug(false),
                _ => println!("Invalid value"),
            },
            None => println!("Value required"),
        }
    }

    fn process_uci(&self) {
        println!("id name Unidexter-R");
        println!("id author Michael Aherne");
        // TODO: send options.
        println!("uciok")
    }

    fn process_isready(&mut self) {
        self.engine.init();
        println!("readyok");
    }

    fn process_setoption(&self, args: &VecDeque<&str>) {
        // TODO: Parse string and set args.
        _ = args;
    }

    fn process_register(&self, args: &VecDeque<&str>) {
        // TODO: Parse string and implement.
        _ = args;
    }

    fn process_ucinewgame(&mut self) {
        self.engine.new_game();
        println!("isready");
    }

    fn process_position(&mut self, args: &mut VecDeque<&str>) {
        let pos_type_opt = args.pop_front();

        if pos_type_opt == None {
            return;
        }

        let pos_type = pos_type_opt.unwrap();

        if pos_type == "startpos" {
            set_startpos(&mut self.engine.position);
        } else if pos_type == "fen" {
            let mut fen_string = String::new();

            while args.front() != None && "moves" != *args.front().unwrap() {
                let arg = args.pop_front().unwrap();

                fen_string.push_str(arg);
                fen_string.push_str(" ");
            }

            set_from_fen(&mut self.engine.position, &fen_string).unwrap();
        } else {
            return;
        }

        if args.len() == 0 {
            return;
        }

        if "moves" != args.pop_front().unwrap() {
            // Something is wrong here.
            return;
        }

        let mut moves: Vec<LongAlgebraicNotationMove> = Vec::new();
        for move_text in args {
            let m = LongAlgebraicNotationMove::from_text(move_text.to_string());
            match m {
                Ok(m) => moves.push(m),
                Err(_e) => println!("Invalid move {0}", move_text),
            }
        }

        make_moves(&mut self.engine.position, &moves);

        println!("{:#?}", self.engine.position);
    }

    fn process_go(&self, args: &VecDeque<&str>) {
        // TODO: Process args and run.
        _ = args;
    }

    fn process_stop(&self) {}

    fn process_ponderhit(&self) {}

    pub fn run(&mut self) {
        loop {
            let mut input = String::new();

            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");

            let input = input.trim();

            match input {
                "quit" => break,
                other => self.process_command(other),
            }
        }
    }
}
