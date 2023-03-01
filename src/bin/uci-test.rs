use std::sync::mpsc::{self, Receiver, Sender};

use chess_uci::messages::{InputMessage, OutputMessage};

pub fn main() {
    let (output_sender, output_receiver) = mpsc::channel::<OutputMessage>();

    let input_receiver = chess_uci::listen(output_receiver);

    fake_engine(output_sender, input_receiver);

}

fn fake_engine(output_sender: Sender<OutputMessage>, input_receiver: Receiver<InputMessage>) {
    loop {
        let input = input_receiver.recv();
        match input {
            Ok(input_message) => {
                match input_message {
                    InputMessage::Quit => {
                        // Important - this tells the output thread to terminate.
                        output_sender.send(OutputMessage::Quitting).expect("sends");
                        return;
                    },
                    InputMessage::SendId => output_sender.send(OutputMessage::Id(vec![("name".to_owned(), "Dexy".to_owned()), ("author".to_owned(), "Michael Aherne".to_owned())])).expect("id sent"),
                    InputMessage::SetDebug(val) => println!("Engine: set debug to {}", val),
                    InputMessage::SetStartPosition => todo!(),
                    InputMessage::SetPositionFromFen(_) => todo!(),
                    InputMessage::MakeMoves(_) => todo!(),
                    InputMessage::GetAvailableOptions => todo!(),
                    InputMessage::IsReady => output_sender.send(OutputMessage::Ready).expect("message sent"),
                    InputMessage::NewGame => todo!(),
                    InputMessage::Go(_) => todo!(),
                    InputMessage::Stop(_) => todo!(),
                    InputMessage::SetOption(_, _) => todo!(),
                    InputMessage::PonderHit => todo!(),
                }
            }
            Err(_) => {}
        }
    }
}
