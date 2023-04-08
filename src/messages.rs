use chess_uci::messages::LongAlgebraicNotationMove;

pub enum InputMessage {
    Quit,
    SetDebug(bool),
    SetStartPosition,
    SetPositionFromFen(String),
    MakeMoves(Vec<LongAlgebraicNotationMove>),
    GetAvailableOptions,
    IsReady,
    NewGame,
    Go(Vec<GoSubcommand>),
    Stop(bool), // argument is whether to send the best move or not
}

pub enum OutputMessage {
    AvailableOptions(Vec<AvailableOption>),
    Ready,
    Quitting,
    BestMove(LongAlgebraicNotationMove, Option<LongAlgebraicNotationMove>),
    Info(Vec<InfoMessage>),
}

pub enum InfoMessage {
    Depth(usize),
    SelectiveDepth(usize),
    TimeSearched(usize),
    NodesSearched(usize),
    PrincipalVariation(Vec<LongAlgebraicNotationMove>),
    // What is multipv?
    Score(Vec<ScoreInfo>),
    CurrentMove(Move0x88),
    CurrentMoveNumber(usize),
    HashFull(usize), // What is this?
    NodesPerSecond(usize),
    TablebaseHits(usize),
    // Do we need sbhits - Shredder database?
    CpuLoad(usize),
    String(String),
    Refutation(Move0x88, Vec<Move0x88>),
    CurrentLine(usize, Vec<Move0x88>),
}

pub enum ScoreInfo {
    Centipawns(Score),
    Mate(usize),
    LowerBound,
    UpperBound,
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
    Infinite,
}

pub struct AvailableOption {}
