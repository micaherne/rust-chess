use crate::position0x88::notation::LongAlgebraicNotationMove;

pub enum InputMessage {
    Quit,
    SetDebug(bool),
    SetStartPosition,
    SetPositionFromFen(String),
    MakeMoves(Vec<LongAlgebraicNotationMove>),
    GetAvailableOptions,
    IsReady,
    NewGame
}

pub enum OutputMessage {
    AvailableOptions(Vec<AvailableOption>),
    Ready,
    Quitting
}

pub struct AvailableOption {

}