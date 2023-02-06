use crate::position0x88::notation::LongAlgebraicNotationMove;

pub enum InputMessage {
    Quit,
    SetDebug(bool),
    SetStartPosition,
    SetPositionFromFen(String),
    MakeMoves(Vec<LongAlgebraicNotationMove>),
    GetAvailableOptions,
    IsReady,
    NewGame,
    Go(Vec<GoSubcommand>)
}

pub enum OutputMessage {
    AvailableOptions(Vec<AvailableOption>),
    Ready,
    Quitting,
    BestMove(LongAlgebraicNotationMove, Option<LongAlgebraicNotationMove>)
}

pub enum GoSubcommand {
    SearchMoves(Vec<LongAlgebraicNotationMove>),
    Ponder,
    WTime(u64),
    BTime(u64),
    WInc(u64),
    BInc(u64),
    MovesToGo(u64),
    Depth(u64),
    Nodes(u64),
    Mate(u64),
    MoveTime(u64),
    Infinite
}


pub struct AvailableOption {

}