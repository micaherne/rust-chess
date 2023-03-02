use chess_uci::messages::{LongAlgebraicNotationMove, InputMessage, OutputMessage};

use crate::{
    engine::Engine,
    position0x88::notation::{make_moves, set_from_fen, set_startpos},
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

trait ToUciString {
    fn to_uci_string(&self) -> String;
}

impl UciOutputListener {
    pub fn listen(self) {
        loop {
            let message = self.receiver.recv();
            match message {
                Err(_) => println!("Error"),
                Ok(OutputMessage::Quitting) => break,
                Ok(m) => self.process_message(m),
            }
        }
    }

    fn process_message(&self, message: OutputMessage) {
        match message {
            OutputMessage::AvailableOptions(m) => self.process_available_options(m),
            OutputMessage::Ready => println!("readyok"),
            OutputMessage::Quitting => {}
            OutputMessage::BestMove(mv, ponder) => {
                if ponder.is_none() {
                    println!("bestmove {}", mv.text)
                } else {
                    println!("bestmove {} ponder {}", mv.text, ponder.unwrap().text)
                }
                
            }
            OutputMessage::Info(info_messages) => {
                let info_string = format!(
                    "{}",
                    info_messages
                        .iter()
                        .map(|m| m.to_uci_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                );
                println!("info {}", info_string)
            }
            OutputMessage::Id(_) => todo!(),
        }
    }

    fn process_available_options(&self, _options: Vec<AvailableOption>) {
        for _option in _options {}
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
        println!("id name Dexy");
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

    fn process_setoption(&self, _args: &mut VecDeque<&str>) {
        todo!()
    }

    fn process_register(&self, _args: &mut VecDeque<&str>) {
        todo!()
    }

    fn process_ucinewgame(&self) {
        self.sender.send(InputMessage::NewGame).unwrap();
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
        let subcommand_tokens = [
            "searchmoves",
            "ponder",
            "wtime",
            "btime",
            "winc",
            "binc",
            "movestogo",
            "depth",
            "nodes",
            "mate",
            "movetime",
            "infininte",
        ];
        let mut subcommands: Vec<GoSubcommand> = vec![];
        loop {
            let subcommand_name = args.pop_front();
            if subcommand_name == None {
                break;
            }

            let subcommand = subcommand_name.unwrap_or("invalid");
            if subcommand == "invalid" {
                break;
            }

            if !subcommand_tokens.contains(&subcommand) {
                break;
            }

            let mut subcommand_params: Vec<&str> = vec![];

            while let Some(arg) = args.pop_front() {
                if subcommand_tokens.contains(&arg) {
                    args.push_front(arg);
                    break;
                }
                subcommand_params.push(arg);
            }

            match subcommand {
                "searchmoves" => {
                    let mut moves: Vec<LongAlgebraicNotationMove> = vec![];
                    for m in subcommand_params {
                        moves.push(LongAlgebraicNotationMove::from_text(m.to_string()).unwrap());
                    }
                    subcommands.push(GoSubcommand::SearchMoves(moves));
                }
                "ponder" => subcommands.push(GoSubcommand::Ponder),
                "wtime" => subcommands.push(GoSubcommand::WTime(
                    subcommand_params[0].parse::<u64>().unwrap(),
                )),
                "btime" => subcommands.push(GoSubcommand::BTime(
                    subcommand_params[0].parse::<u64>().unwrap(),
                )),
                "winc" => subcommands.push(GoSubcommand::WInc(
                    subcommand_params[0].parse::<u64>().unwrap(),
                )),
                "binc" => subcommands.push(GoSubcommand::BInc(
                    subcommand_params[0].parse::<u64>().unwrap(),
                )),
                "movestogo" => subcommands.push(GoSubcommand::MovesToGo(
                    subcommand_params[0].parse::<u64>().unwrap(),
                )),
                "depth" => subcommands.push(GoSubcommand::Depth(
                    subcommand_params[0].parse::<u64>().unwrap(),
                )),
                "nodes" => subcommands.push(GoSubcommand::Nodes(
                    subcommand_params[0].parse::<u64>().unwrap(),
                )),
                "mate" => subcommands.push(GoSubcommand::Mate(
                    subcommand_params[0].parse::<u64>().unwrap(),
                )),
                "movetime" => subcommands.push(GoSubcommand::MoveTime(
                    subcommand_params[0].parse::<u64>().unwrap(),
                )),
                "infinite" => subcommands.push(GoSubcommand::Infinite),
                _ => {}
            }
        }

        let message: InputMessage = InputMessage::Go(subcommands);

        self.sender.send(message).unwrap();
    }

    fn process_stop(&self) {
        self.sender.send(InputMessage::Stop(true)).unwrap();
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

impl ToUciString for InfoMessage {
    fn to_uci_string(&self) -> String {
        match self {
            InfoMessage::Depth(x) => format!("depth {}", x),
            InfoMessage::SelectiveDepth(x) => format!("seldepth {}", x),
            InfoMessage::TimeSearched(x) => format!("time {}", x),
            InfoMessage::NodesSearched(x) => format!("nodes {}", x),
            InfoMessage::PrincipalVariation(pv) => format!(
                "pv {}",
                pv.iter()
                    .map(|m| m.text.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            InfoMessage::Score(score_info) => format!(
                "score {}",
                score_info
                    .iter()
                    .map(|m| m.to_uci_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            InfoMessage::CurrentMove(x) => {
                let mv: LongAlgebraicNotationMove = x.into();
                format!("currmove {}", mv.text)
            }
            InfoMessage::CurrentMoveNumber(x) => format!("currmove {}", x),
            InfoMessage::HashFull(x) => format!("hashfull {}", x),
            InfoMessage::NodesPerSecond(x) => format!("nps {}", x),
            InfoMessage::TablebaseHits(x) => format!("tbhits {}", x),
            InfoMessage::CpuLoad(x) => format!("cpuload {}", x),
            InfoMessage::String(x) => format!("string {}", x),
            InfoMessage::Refutation(_, _) => todo!(),
            InfoMessage::CurrentLine(_, _) => todo!(),
        }
    }
}

impl ToUciString for ScoreInfo {
    fn to_uci_string(&self) -> String {
        match self {
            ScoreInfo::Centipawns(x) => format!("cp {}", x),
            ScoreInfo::Mate(x) => format!("mate {}", x),
            ScoreInfo::LowerBound => "lowerbound".to_string(),
            ScoreInfo::UpperBound => "upperbound".to_string(),
        }
    }
}
