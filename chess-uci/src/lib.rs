use std::{
    collections::HashMap,
    io,
    ops::Deref,
    str::{FromStr, SplitAsciiWhitespace},
    sync::mpsc::{self, Receiver},
    thread,
};

use regex::Regex;

use messages::{
    AvailableOption, GoSubcommand, InfoMessage, InputMessage, Line, LongAlgebraicNotationMove,
    MoveList, OptionType, OutputMessage, ScoreInfo,
};

pub mod messages;

pub enum InputError {
    UnableToParse,
}

pub trait ToUciString {
    fn to_uci_string(&self) -> String;
}

#[derive(Debug)]
pub enum UciInputError {
    Empty,
    UnknownKeyword(String),
    InvalidFormat,
    NotImplemented,
}

struct InputMessages(Vec<InputMessage>);

impl Deref for InputMessages {
    type Target = Vec<InputMessage>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl FromStr for InputMessages {
    type Err = UciInputError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut token_iter = s.trim().split_ascii_whitespace();
        let keyword = token_iter.next().ok_or(UciInputError::Empty)?;
        match keyword {
            "uci" => Ok(InputMessages(vec![InputMessage::SendId])),
            "debug" => {
                let val = token_iter.next().ok_or(UciInputError::InvalidFormat)?;
                let b = match val {
                    "on" => true,
                    "off" => false,
                    _ => return Err(UciInputError::InvalidFormat),
                };
                Ok(InputMessages(vec![InputMessage::SetDebug(b)]))
            }
            "isready" => Ok(InputMessages(vec![InputMessage::IsReady])),
            "setoption" => {
                let params = parse_params(token_iter, vec!["name", "value"]);
                if !params.contains_key("name") {
                    return Err(UciInputError::InvalidFormat);
                }
                let value = if params.contains_key("value") {
                    Some(params["value"].to_string())
                } else {
                    None
                };
                Ok(InputMessages(vec![InputMessage::SetOption(
                    params["name"].to_string(),
                    value,
                )]))
            }
            "register" => Err(UciInputError::NotImplemented),
            "ucinewgame" => Ok(InputMessages(vec![InputMessage::NewGame])),
            "position" => {
                let mut messages = vec![];
                let pos_type = token_iter.next();
                match pos_type {
                    Some(t) => match t {
                        "startpos" => messages.push(InputMessage::SetStartPosition),
                        "fen" => {
                            let fen_strings: Vec<&str> = (&mut token_iter).take(6).collect();
                            let fen_string = fen_strings.join(" ");
                            messages.push(InputMessage::SetPositionFromFen(fen_string))
                        }
                        x => return Err(UciInputError::UnknownKeyword(x.to_owned())),
                    },
                    None => return Err(UciInputError::InvalidFormat),
                }
                let moves_token = token_iter.next();
                match moves_token {
                    Some("moves") => {
                        let mut moves: Line = vec![];
                        for token in token_iter {
                            let m = token.parse::<LongAlgebraicNotationMove>()?;
                            moves.push(m)
                        }
                        messages.push(InputMessage::MakeMoves(moves));
                    }
                    Some(_) => return Err(UciInputError::InvalidFormat),
                    None => {}
                }
                Ok(InputMessages(messages))
            }
            "go" => {
                let go_params = parse_params(
                    token_iter,
                    vec![
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
                        "infinite",
                    ],
                );
                let mut subcommands = vec![];
                for (kw, str) in go_params {
                    let command: GoSubcommand = format!("{} {}", kw, str).parse()?;
                    subcommands.push(command);
                }
                Ok(InputMessages(vec![InputMessage::Go(subcommands)]))
            }
            "stop" => Ok(InputMessages(vec![InputMessage::Stop(true)])),
            "ponderhit" => Ok(InputMessages(vec![InputMessage::PonderHit])),
            "quit" => Ok(InputMessages(vec![InputMessage::Quit])),
            kw => return Err(UciInputError::UnknownKeyword(kw.into())),
        }
    }
}

impl FromStr for GoSubcommand {
    type Err = UciInputError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut token_iter = s.split_ascii_whitespace();
        let keyword = token_iter.next().ok_or(UciInputError::Empty)?;
        match keyword {
            "searchmoves" => {
                let mut move_list = MoveList::new();
                for mv in token_iter {
                    move_list.push(LongAlgebraicNotationMove::from_string(mv));
                }
                Ok(GoSubcommand::SearchMoves(move_list))
            }
            "ponder" => Ok(GoSubcommand::Ponder),
            "wtime" => Ok(GoSubcommand::WTime(next_token_as_number(&mut token_iter)?)),
            "btime" => Ok(GoSubcommand::BTime(next_token_as_number(&mut token_iter)?)),
            "winc" => Ok(GoSubcommand::WInc(next_token_as_number(&mut token_iter)?)),
            "binc" => Ok(GoSubcommand::BInc(next_token_as_number(&mut token_iter)?)),
            "movestogo" => Ok(GoSubcommand::MovesToGo(next_token_as_number(
                &mut token_iter,
            )?)),
            "depth" => Ok(GoSubcommand::Depth(next_token_as_number(&mut token_iter)?)),
            "nodes" => Ok(GoSubcommand::Nodes(next_token_as_number(&mut token_iter)?)),
            "mate" => Ok(GoSubcommand::Mate(next_token_as_number(&mut token_iter)?)),
            "movetime" => Ok(GoSubcommand::MoveTime(next_token_as_number(
                &mut token_iter,
            )?)),
            "infinite" => Ok(GoSubcommand::Infinite),
            kw => Err(UciInputError::UnknownKeyword(kw.into())),
        }
    }
}

impl FromStr for LongAlgebraicNotationMove {
    type Err = UciInputError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // TODO: Validate notation here.
        Ok(LongAlgebraicNotationMove::from_string(s))
    }
}

fn next_token_as_number<T: FromStr>(
    token_iter: &mut SplitAsciiWhitespace,
) -> Result<T, UciInputError> {
    let thing = token_iter.next().ok_or(UciInputError::InvalidFormat)?;
    let parsed = thing.parse::<T>().or(Err(UciInputError::InvalidFormat))?;
    Ok(parsed)
}

fn parse_params(
    token_iterator: SplitAsciiWhitespace,
    keywords: Vec<&str>,
) -> HashMap<String, String> {
    let mut result = HashMap::new();

    let mut current_keyword: Option<String> = None;
    let mut current_strings = vec![];

    for token in token_iterator {
        if keywords.contains(&token) {
            if current_keyword.is_some() {
                result.insert(
                    current_keyword.unwrap().to_string(),
                    current_strings.join(" "),
                );
                current_strings.clear();
            }
            current_keyword = Some(token.to_string());
        } else {
            current_strings.push(token.to_owned());
        }
    }
    if current_keyword.is_some() {
        let kw = current_keyword.unwrap();
        if !result.contains_key(&kw) {
            result.insert(kw, current_strings.join(" "));
        }
    }

    result
}

impl ToUciString for OutputMessage {
    fn to_uci_string(&self) -> String {
        match self {
            OutputMessage::Id(parts) => {
                let strings: Vec<String> = parts
                    .iter()
                    .map(|x| format!("id {} {}", x.0, x.1))
                    .collect();
                strings.join("\n")
            }
            OutputMessage::AvailableOptions(available_options) => {
                let lines: Vec<String> = available_options
                    .iter()
                    .map(|opt| opt.to_uci_string())
                    .collect();
                lines.join("\n")
            }
            OutputMessage::Ready => "readyok".to_owned(),
            OutputMessage::Quitting => "".to_owned(),
            OutputMessage::BestMove(best, ponder) => {
                let mut result = format!("bestmove {}", best.to_uci_string());
                if let Some(p) = ponder {
                    result = format!("{} ponder {}", result, p.to_uci_string());
                }
                result
            }
            OutputMessage::Info(parts) => {
                let body: Vec<String> = parts.iter().map(|x| x.to_uci_string()).collect();
                format!("info {}", body.join(" "))
            }
            OutputMessage::UciOk => format!("uciok"),
        }
    }
}

impl ToUciString for LongAlgebraicNotationMove {
    fn to_uci_string(&self) -> String {
        self.text.to_owned()
    }
}

impl ToUciString for MoveList {
    fn to_uci_string(&self) -> String {
        let strs: Vec<String> = self.iter().map(|mv| mv.to_uci_string()).collect();
        strs.join(" ")
    }
}

impl ToUciString for Vec<ScoreInfo> {
    fn to_uci_string(&self) -> String {
        let strs: Vec<String> = self.iter().map(|mv| mv.to_uci_string()).collect();
        strs.join(" ")
    }
}

impl ToUciString for InfoMessage {
    fn to_uci_string(&self) -> String {
        match self {
            InfoMessage::Depth(x) => format!("depth {}", x),
            InfoMessage::SelectiveDepth(x) => format!("seldepth {}", x),
            InfoMessage::TimeSearched(x) => format!("time {}", x),
            InfoMessage::NodesSearched(x) => format!("nodes {}", x),
            InfoMessage::PrincipalVariation(moves) => format!("pv {}", moves.to_uci_string()),
            InfoMessage::Score(score) => format!("score {}", score.to_uci_string()),
            InfoMessage::CurrentMove(mv) => format!("currmove {}", mv.to_uci_string()),
            InfoMessage::CurrentMoveNumber(x) => format!("currmovenumber {}", x),
            InfoMessage::HashFull(x) => format!("hashfull {}", x),
            InfoMessage::NodesPerSecond(x) => format!("nps {}", x),
            InfoMessage::TablebaseHits(x) => format!("tbhits {}", x),
            InfoMessage::CpuLoad(x) => format!("cpuload {}", x),
            InfoMessage::String(x) => format!("string {}", x),
            // TODO: Should we try and trim this if there are no refutations?
            InfoMessage::Refutation(mv, refutations) => format!(
                "refutation {} {}",
                mv.to_uci_string(),
                refutations.to_uci_string()
            ),
            InfoMessage::CurrentLine(cpu, line) => {
                let mut result = match cpu {
                    Some(c) => format!("{} ", c),
                    None => "".to_owned(),
                };
                result += line.to_uci_string().as_str();
                result
            }
        }
    }
}

impl ToUciString for ScoreInfo {
    fn to_uci_string(&self) -> String {
        match self {
            ScoreInfo::Centipawns(x) => format!("cp {}", x),
            ScoreInfo::Mate(x) => format!("mate {}", x),
            ScoreInfo::LowerBound => format!("lowerbound"),
            ScoreInfo::UpperBound => format!("upperbound"),
        }
    }
}

impl ToUciString for OptionType {
    fn to_uci_string(&self) -> String {
        match self {
            OptionType::Check => "check".to_string(),
            OptionType::Spin => "spin".to_string(),
            OptionType::Combo => "combo".to_string(),
            OptionType::Button => "button".to_string(),
            OptionType::String => "string".to_string(),
        }
    }
}

impl ToUciString for AvailableOption {
    fn to_uci_string(&self) -> String {
        let mut result = String::new();
        result.push_str("option ");
        result.push_str(format!("name {} ", self.name).as_str());
        result.push_str(format!("type {} ", self.opt_type.to_uci_string()).as_str());
        if self.default.is_some() {
            result.push_str(format!("default {}", self.default.unwrap()).as_str());
        }
        if self.min.is_some() {
            result.push_str(format!("min {} ", self.min.unwrap()).as_str());
        }
        if self.max.is_some() {
            result.push_str(format!("max {} ", self.max.unwrap()).as_str());
        }
        for v in self.var.iter() {
            result.push_str(format!("var {} ", v).as_str());
        }
        result = result.trim_end().to_owned();
        result
    }
}

pub fn listen(output_receiver: Receiver<OutputMessage>) -> Receiver<InputMessage> {
    let stdin_receiver = spawn_stdin_channel();
    let input_listener = spawn_input_listener(stdin_receiver);
    spawn_output_listener(output_receiver);
    input_listener
}

fn spawn_input_listener(stdin_receiver: Receiver<String>) -> Receiver<InputMessage> {
    let (tx, rx) = mpsc::channel::<InputMessage>();
    thread::spawn(move || loop {
        if let Ok(text_input) = stdin_receiver.recv() {
            let messages = process_text_input(text_input);
            match messages {
                Ok(msg) => {
                    for input_message in msg.0 {
                        tx.send(input_message).expect("message sent ok")
                    }
                }
                Err(_) => {}
            }
        };
    });
    rx
}

fn spawn_output_listener(output_receiver: Receiver<OutputMessage>) {
    thread::spawn(move || loop {
        match output_receiver.recv() {
            Ok(OutputMessage::Quitting) => {
                return;
            }
            Ok(output_message) => {
                let output = output_message.to_uci_string();
                println!("{}", output);
            }
            Err(_) => {}
        }
    });
}

fn spawn_stdin_channel() -> Receiver<String> {
    let (tx, rx) = mpsc::channel::<String>();
    thread::spawn(move || loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).unwrap();
        let input = &buffer.trim().to_string();
        tx.send(buffer).unwrap();
        if input == "quit" {
            return;
        }
    });
    rx
}

fn process_text_input(text_input: String) -> Result<InputMessages, UciInputError> {
    if text_input.trim() == "quit" {
        return Ok(InputMessages(vec![InputMessage::Quit]));
    }
    let messages = text_input.trim().parse::<InputMessages>()?;
    return Ok(messages);
}

pub fn validate_long_algebraic_move(text: &str) -> bool {
    let re = Regex::new("^([a-h][1-8]){2}[rnbq]?$").unwrap();
    re.is_match(text)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_to_uci_string() {
        assert_eq!(
            "e2e4",
            LongAlgebraicNotationMove::from_string("e2e4").to_uci_string()
        );
        assert_eq!(
            "info currmove e2e4",
            OutputMessage::Info(vec![InfoMessage::CurrentMove(
                LongAlgebraicNotationMove::from_string("e2e4")
            )])
            .to_uci_string()
        );
    }

    #[test]
    fn test_from_uci() {
        let mut x = "uci".parse::<InputMessages>().unwrap();
        assert_eq!(1, x.len());
        // x = "position startpos moves e2e4".parse::<InputMessages>().unwrap();
        // assert_eq!(2, x.len());
        x = "debug off".parse::<InputMessages>().unwrap();
        if let InputMessage::SetDebug(false) = x[0] {
            assert!(true);
        } else {
            assert!(false);
        }
        x = "go wtime 30000 btime 30000 winc 5 binc 5"
            .parse::<InputMessages>()
            .unwrap();
        assert_eq!(1, x.len());
        if let InputMessage::Go(f) = &x[0] {
            assert!(true);
            assert_eq!(4, f.len());
        } else {
            assert!(false);
        }
        x = "position fen rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1 moves e7e5"
            .parse::<InputMessages>()
            .unwrap();
        assert_eq!(2, x.len())
    }

    #[test]
    fn test_parse_params() {
        let v1 = parse_params(
            "name NalimovPath value d:\\tb;c\\tb".split_ascii_whitespace(),
            vec!["name", "value"],
        );
        assert_eq!(2, v1.len());
        assert!(v1.contains_key("name"));
        assert!(v1.contains_key("value"));
    }

    #[test]
    fn test_validate_long_algebraic_move() {
        assert!(validate_long_algebraic_move("e2e4"));
        assert!(validate_long_algebraic_move("e7e8q"));
        assert!(!validate_long_algebraic_move("text"));
    }
}
