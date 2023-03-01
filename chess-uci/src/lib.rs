use std::{
    io,
    str::FromStr,
    sync::mpsc::{self, Receiver},
    thread,
};

use messages::{
    AvailableOption, InfoMessage, InputMessage, LongAlgebraicNotationMove, MoveList, OptionType,
    OutputMessage, ScoreInfo,
};

pub mod messages;

pub enum InputError {
    UnableToParse,
}

pub trait ToUciString {
    fn to_uci_string(&self) -> String;
}

pub enum UciInputError {
    Empty,
    UnknownKeyword(String),
    InvalidFormat,
    NotImplemented,
}

struct InputMessages(Vec<InputMessage>);

impl FromStr for InputMessages {
    type Err = UciInputError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut token_iter = s.trim().split_ascii_whitespace();
        let keyword = token_iter.next().ok_or(UciInputError::Empty)?;
        match keyword {
            "uci" => Ok(InputMessages(vec![InputMessage::SendId])),
            "debug" => {
                let val = token_iter.next().ok_or(UciInputError::InvalidFormat)?;
                match val {
                    "true" => Ok(InputMessages(vec![InputMessage::SetDebug(true)])),
                    "false" => Ok(InputMessages(vec![InputMessage::SetDebug(false)])),
                    _ => Err(UciInputError::InvalidFormat),
                }
            }
            "isready" => Ok(InputMessages(vec![InputMessage::IsReady])),
            "setoption" => Err(UciInputError::NotImplemented),
            "register" => Err(UciInputError::NotImplemented),
            "ucinewgame" => Ok(InputMessages(vec![InputMessage::NewGame])),
            "position" => Err(UciInputError::NotImplemented),
            "go" => Err(UciInputError::NotImplemented),
            "stop" => Ok(InputMessages(vec![InputMessage::Stop(true)])),
            "ponderhit" => Ok(InputMessages(vec![InputMessage::PonderHit])),
            "quit" => Ok(InputMessages(vec![InputMessage::Quit])),
            kw => return Err(UciInputError::UnknownKeyword(kw.into())),
        }
    }
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
                let output = process_received_message(output_message);
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

pub fn process_received_message(message: OutputMessage) -> String {
    message.to_uci_string()
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
            OutputMessage::Info(InfoMessage::CurrentMove(
                LongAlgebraicNotationMove::from_string("e2e4")
            ))
        );
    }
}
