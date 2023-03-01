pub struct LongAlgebraicNotationMove {
    pub text: String,
}

impl LongAlgebraicNotationMove {
    pub fn from_string(str: &str) -> Self {
        Self {
            text: str.to_string(),
        }
    }
}

pub type Score = i32;

pub type Line = Vec<LongAlgebraicNotationMove>;
pub type MoveList = Vec<LongAlgebraicNotationMove>;

pub enum InputMessage {
    SendId, // Uci command.
    SetDebug(bool),
    IsReady,
    SetOption(String, String),
    // Register?
    NewGame,
    SetStartPosition,
    SetPositionFromFen(String),
    MakeMoves(Line),
    GetAvailableOptions,
    Go(Vec<GoSubcommand>),
    Stop(bool), // argument is whether to send the best move or not
    PonderHit,
    Quit,
}

pub enum OutputMessage {
    Id(Vec<(String, String)>),
    AvailableOptions(Vec<AvailableOption>),
    Ready,
    Quitting, // Not a real UCI message, used for telling threads to quit.
    BestMove(LongAlgebraicNotationMove, Option<LongAlgebraicNotationMove>),
    Info(Vec<InfoMessage>),
}

pub enum InfoMessage {
    Depth(usize),
    SelectiveDepth(usize),
    TimeSearched(usize),
    NodesSearched(usize),
    PrincipalVariation(Line),
    // What is multipv?
    Score(Vec<ScoreInfo>),
    CurrentMove(LongAlgebraicNotationMove),
    CurrentMoveNumber(usize),
    HashFull(usize), // What is this?
    NodesPerSecond(usize),
    TablebaseHits(usize),
    // Do we need sbhits - Shredder database?
    CpuLoad(usize),
    String(String),
    Refutation(LongAlgebraicNotationMove, MoveList),
    CurrentLine(Option<usize>, Line),
}

pub enum ScoreInfo {
    Centipawns(Score),
    Mate(usize),
    LowerBound,
    UpperBound,
}

pub enum GoSubcommand {
    SearchMoves(MoveList),
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

pub struct AvailableOption {
    pub name: String,
    pub opt_type: OptionType,
    pub default: Option<usize>,
    pub min: Option<usize>,
    pub max: Option<usize>,
    pub var: Vec<String>
}

pub enum OptionType {
    Check,
    Spin,
    Combo,
    Button,
    String,
}
