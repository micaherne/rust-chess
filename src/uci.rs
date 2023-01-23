use crate::{engine::Engine, board::LongAlgebraicNotationMove, fen::parse_fen};

use std::{collections::VecDeque, io, borrow::Borrow};

pub struct UciInterface {
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
            "uci" => self.process_uci(args),
            "debug" => self.process_debug(args),
            "isready" => self.process_isready(args),
            "setoption" => self.process_setoption(args),
            "register" => self.process_register(args),
            "ucinewgame" => self.process_ucinewgame(args),
            "position" => self.process_position(args),
            "go" => self.process_go(args),
            "stop" => self.process_stop(args),
            "ponderhit" => self.process_ponderhit(args),
            "quit" => {} // Quit followed by any other text should just be ignored.
            _ => panic!("Unknown keyword"),
        }
    }

    fn process_debug(&self, args: &mut VecDeque<&str>) {
        // TODO: This needs to actually set the value!
        match args.pop_front() {
            Some(val) => match val.trim() {
                "on" => println!("Switching debug on"),
                "off" => println!("Switching debug off"),
                _ => println!("Invalid value"),
            },
            None => println!("Value required"),
        }
    }

    fn process_uci(&self, args: &VecDeque<&str>) {
        println!("id name Unidexter-R");
        println!("id author Michael Aherne");
        // TODO: send options.
        println!("uciok")
    }

    fn process_isready(&mut self, args: &VecDeque<&str>) {
        self.engine.init();
        println!("readyok");
    }

    fn process_setoption(&self, args: &VecDeque<&str>) {}

    fn process_register(&self, args: &VecDeque<&str>) {}

    fn process_ucinewgame(&self, args: &VecDeque<&str>) {}

    fn process_position(&mut self, args: &mut VecDeque<&str>) {
        let pos_type_opt = args.pop_front();

        if pos_type_opt == None {
            return;
        }

        let pos_type = pos_type_opt.unwrap();

        if pos_type == "startpos" {
            self.engine.board.set_startpos();
        } else if pos_type == "fen" {
            let mut fen_string = String::new();
            
            while args.front() != None && "moves" != *args.front().unwrap() {
                let arg = args.pop_front().unwrap();

                fen_string.push_str(arg);
                fen_string.push_str(" ");
            }

            let parsed_fen = parse_fen(&fen_string);

            match parsed_fen {
                Err(_fen_error) => return,
                Ok(fen) => self.engine.board.set_from_fen(fen)
            }
            
            
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
            moves.push(LongAlgebraicNotationMove::from_text(move_text.to_string()));
        }

        self.engine.board.make_moves(&moves);

        println!("{:#?}", self.engine.board);

    }

    fn process_go(&self, args: &VecDeque<&str>) {}

    fn process_stop(&self, args: &VecDeque<&str>) {}

    fn process_ponderhit(&self, args: &VecDeque<&str>) {}

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
