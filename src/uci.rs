use std::{
    io::{self, BufRead},
    sync::{atomic::AtomicBool, Arc},
    time::Duration,
};

use regex::Regex;
use thiserror::Error;

use crate::{
    depth::Depth,
    notation::parse_coordinate_notation,
    options::{Options, ReadOnlyOptions},
    piece::Color,
    position::Position,
    search::{ProgressType, Search},
    time::{TimeControl, TimeManager},
    tt::TranspositionTable,
};

/// Entry point for the UCI (Universal Chess Interface) protocol.
///
/// This function initializes the engine and creates a UCI interface, then hands over control to the UCI protocol
/// handler which will process commands from the GUI and respond appropriately.
///
/// The function will continue running until it receives a "quit" command from the GUI.
///
/// # Parameters
/// * `tt_size` - Size of the transposition table in megabytes. This must be a power of 2.
pub fn run_uci(tt_size: usize) {
    let engine = UciEngine::new(tt_size);
    let uci = Uci::new(engine);
    uci.run();
}

/// Error types that can occur during UCI protocol handling.
///
/// These errors represent various failure conditions when parsing and executing UCI commands from the GUI. Each variant
/// includes contextual information to help diagnose the issue.
#[derive(Error, Debug)]
#[allow(clippy::enum_variant_names)]
enum UciError {
    /// Returned when the "position" command has invalid syntax or arguments.
    #[error("Invalid position command: {0}")]
    InvalidPositionCommand(String),

    /// Returned when a provided FEN string does not represent a valid chess position.
    #[error("Invalid FEN string: {0}")]
    InvalidFenString(String),

    /// Returned when a move string cannot be parsed or is illegal in the current position.
    #[error("Invalid move: {0}")]
    InvalidMove(String),

    /// Returned when the size of the hash table is invalid or not supported.
    #[error("Invalid hash size: {0}")]
    InvalidHashSize(String),

    /// Returned when an invalid value is provided for a UCI option.
    #[error("Invalid option value for {option_name}: {value}")]
    InvalidOptionValue { option_name: String, value: String },

    /// Returned when an unexpected token is encountered during command parsing. Includes the received token and what
    /// was expected instead.
    #[error("Invalid token: {token}, expected: {expected}")]
    InvalidToken { token: String, expected: &'static str },
}

/// Represents the different types of UCI options that can be set in a chess engine.
///
/// UCI (Universal Chess Interface) protocol defines various options that can be configured by the GUI or user. This
/// enum encapsulates all possible option types with their associated data.
#[allow(dead_code)]
enum UciOptionType {
    /// A boolean option (true/false) The wrapped value represents the default setting
    Check(bool),

    /// A numeric option with constraints Contains default value and allowed range (min/max)
    Spin { default: u64, min: u64, max: u64 },

    /// A selection from a predefined list of string values Contains the default value and all possible choices
    Combo { default: String, values: Vec<String> },

    /// A button-type option that can be "pressed" but has no value
    Button,

    /// A free-form text option The wrapped value represents the default string
    String(String),
}

/// Handles the UCI protocol communication between a chess GUI and the engine.
///
/// This struct acts as a bridge that translates UCI protocol commands from the GUI into engine operations and sends
/// appropriate responses back. It manages the protocol state and message flow according to the UCI specification.
struct Uci {
    /// The underlying chess engine that performs the actual calculations.
    engine: UciEngine,
}

impl Uci {
    /// Creates a new UCI protocol handler with the provided engine.
    ///
    /// This initializes a UCI interface that will use the given engine for processing chess operations like position
    /// evaluation and move search.
    ///
    /// # Parameters
    /// * `engine` - The chess engine instance that will be used for calculations
    ///
    /// # Returns
    /// A new UCI protocol handler ready to process commands
    fn new(engine: UciEngine) -> Self {
        Uci { engine }
    }

    /// Starts the UCI protocol's main command processing loop.
    ///
    /// This function reads commands from standard input, parses them according to the UCI protocol specification, and
    /// delegates the processing to appropriate handlers. It continues running until a "quit" command is received.
    ///
    /// If an error occurs during command processing, it will be reported through the UCI protocol's info mechanism.
    fn run(mut self) {
        let mut stdin = io::stdin().lock();

        let mut input = String::new();
        loop {
            stdin.read_line(&mut input).expect("Should be able to read from stdin");
            let tokens: Vec<&str> = input.split_whitespace().collect();
            let result = match tokens.first() {
                Some(&"uci") => self.handle_uci(),
                Some(&"setoption") => self.handle_setoption(tokens.as_slice()),
                Some(&"isready") => self.handle_isready(),
                Some(&"position") => self.handle_position(tokens.as_slice()),
                Some(&"go") => self.handle_go(tokens.as_slice()),
                Some(&"stop") => self.handle_stop(),
                Some(&"quit") => break,
                Some(&command) => {
                    Self::send_unknown_command(command);
                    Ok(())
                }
                None => Ok(()),
            };

            if let Err(e) = result {
                Self::send_info_string(&e.to_string());
            }

            input.clear();
        }
    }

    //==================================================================================================================
    // UCI commands from the GUI to the engine
    //==================================================================================================================

    /// Processes the "uci" command from the GUI.
    ///
    /// When a GUI sends the "uci" command, this method initializes the UCI protocol by delegating to the underlying
    /// engine. The engine will typically respond by sending identification information, available options, and finally
    /// the "uciok" message to indicate readiness.
    ///
    /// This is usually the first command sent by the GUI during the startup phase of communication.
    ///
    /// # Returns
    /// * `Ok(())` - If the command was processed successfully
    /// * `Err(UciError)` - If an error occurred during processing
    fn handle_uci(&mut self) -> Result<(), UciError> {
        self.engine.handle_uci()
    }

    /// Processes the "setoption" command from the GUI according to UCI protocol.
    ///
    /// Parses a sequence of tokens from a "setoption" UCI command to extract the option name and value. The format
    /// expected is "setoption [name <option name>] [value <option value>]".
    ///
    /// # Parameters
    /// * `tokens` - Slice of string slices containing the tokenized UCI command (first token should be "setoption")
    ///
    /// # Returns
    /// * `Ok(())` if option was processed successfully
    /// * `Err(UciError)` if an error occurred during processing
    fn handle_setoption(&mut self, tokens: &[&str]) -> Result<(), UciError> {
        debug_assert!(!tokens.is_empty(), "Tokens should not be empty");
        debug_assert!(tokens[0] == "setoption", "First token should be 'setoption'");

        let mut next_token_index = 1;

        // Read the option name
        let mut name = String::new();
        loop {
            if tokens.len() <= next_token_index || tokens[next_token_index] == "value" {
                break;
            }

            if tokens[next_token_index] != "name" {
                name.push_str(tokens[next_token_index]);
            }

            next_token_index += 1;
        }

        // Read the option value
        let mut value = String::new();
        loop {
            if tokens.len() <= next_token_index {
                break;
            }

            if tokens[next_token_index] != "value" {
                value.push_str(tokens[next_token_index]);
            }

            next_token_index += 1;
        }

        self.engine.handle_setoption(&name, &value)
    }

    /// Processes the "isready" command from the GUI.
    ///
    /// This method responds to the GUI's readiness check by delegating to the underlying engine. Once the engine has
    /// completed any pending initialization tasks, it will respond with "readyok" to signal that it's ready to accept
    /// further commands.
    ///
    /// The GUI typically sends this command to synchronize with the engine before sending commands that require the
    /// engine to be in a ready state.
    ///
    /// # Returns
    /// * `Ok(())` - If the command was processed successfully
    /// * `Err(UciError)` - If an error occurred during processing
    fn handle_isready(&mut self) -> Result<(), UciError> {
        self.engine.handle_isready()
    }

    /// Processes the "position" command from the GUI.
    ///
    /// This method sets up the chess position for the engine based on the command format:
    /// - "position startpos [moves ...]" - Sets up the initial position with optional moves
    /// - "position fen [FEN string] [moves ...]" - Sets up a position from a FEN string with optional moves
    ///
    /// The position can be followed by a sequence of moves to apply to the starting position.
    ///
    /// # Parameters
    /// * `tokens` - Command tokens from the GUI, where the first token must be "position"
    ///
    /// # Returns
    /// * `Ok(())` - If the position was set successfully
    /// * `Err(UciError)` - If the command format is invalid or the position couldn't be set
    ///
    /// # Errors
    /// * `UciError::InvalidPositionCommand` - If the command is missing arguments or has invalid format
    /// * `UciError::InvalidFenString` - If the provided FEN string is invalid
    fn handle_position(&mut self, tokens: &[&str]) -> Result<(), UciError> {
        debug_assert!(!tokens.is_empty(), "Tokens should not be empty");
        debug_assert!(tokens[0] == "position", "First token should be 'position'");

        let mut next_token_index = 1;

        if tokens.len() < 2 {
            return Err(UciError::InvalidPositionCommand("No arguments provided".to_string()));
        }

        // Read the position (startpos or fen)
        let fen: String = match tokens[next_token_index] {
            "startpos" => {
                const DEFAULT_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
                next_token_index += 1;
                DEFAULT_FEN.to_string()
            }
            "fen" => {
                const FEN_TOKENS: usize = 6;
                if tokens.len() < next_token_index + FEN_TOKENS {
                    return Err(UciError::InvalidFenString(tokens[next_token_index..].join(" ")));
                }
                next_token_index += 1;
                let fen = tokens[next_token_index..next_token_index + FEN_TOKENS].join(" ");
                next_token_index += FEN_TOKENS;
                fen
            }
            _ => {
                return Err(UciError::InvalidPositionCommand(tokens[next_token_index].to_string()));
            }
        };

        // Read the moves if any
        let mut moves = Vec::new();
        if next_token_index < tokens.len() && tokens[next_token_index] == "moves" {
            next_token_index += 1;
            moves = tokens[next_token_index..].to_vec();
        }

        self.engine.handle_position(fen, &moves)
    }

    /// Extracts valid chess move strings from the provided tokens.
    ///
    /// Parses a sequence of tokens and collects all valid chess move strings that match the standard algebraic notation
    /// pattern (e.g., "e2e4", "g8f6", "e7e8q"). Stops collecting once it encounters a token that doesn't match the move
    /// pattern.
    ///
    /// # Parameters
    /// * `tokens` - A slice of string tokens that might contain chess moves
    ///
    /// # Returns
    /// A vector of validated chess move strings
    fn read_search_moves(tokens: &[&str]) -> Vec<String> {
        let re = Regex::new(r"^([a-h][1-8]){2}[qrbn]?$").expect("The regex should be valid");
        tokens.iter().take_while(|mv_str| re.is_match(mv_str)).map(|str| str.to_string()).collect()
    }

    /// Processes the "go" command from the GUI to start the engine search.
    ///
    /// This method parses the various search parameters specified by the GUI and initiates the search for the best move
    /// in the current position. The command can include multiple options that control the search behavior such as time
    /// limits, search depth, and move restrictions.
    ///
    /// # Parameters
    /// * `tokens` - Command tokens from the GUI, where the first token must be "go"
    ///
    /// # Returns
    /// * `Ok(())` - If the search was initiated successfully
    /// * `Err(UciError)` - If the command format is invalid or parameters couldn't be parsed
    ///
    /// # Supported Options
    /// * `searchmoves [moves...]` - Restricts the search to the specified moves only
    /// * `ponder` - Puts the engine into pondering mode
    /// * `wtime/btime [ms]` - Remaining time for white/black in milliseconds
    /// * `winc/binc [ms]` - Time increment for white/black after each move in milliseconds
    /// * `movestogo [n]` - Number of moves to the next time control
    /// * `depth [n]` - Maximum search depth
    /// * `nodes [n]` - Maximum number of nodes to search
    /// * `mate [n]` - Search for mate in n moves
    /// * `movetime [ms]` - Maximum time to spend searching in milliseconds
    /// * `infinite` - Search until a "stop" command is received
    ///
    /// # Errors
    /// * `UciError::InvalidToken` - If a parameter value can't be parsed correctly
    fn handle_go(&mut self, tokens: &[&str]) -> Result<(), UciError> {
        debug_assert!(!tokens.is_empty(), "Tokens should not be empty");
        debug_assert!(tokens[0] == "go", "First token should be 'go'");

        let mut search_moves: Option<Vec<String>> = None;
        let mut ponder = false;
        let mut white_time: Option<Duration> = None;
        let mut black_time: Option<Duration> = None;
        let mut white_inc: Option<Duration> = None;
        let mut black_inc: Option<Duration> = None;
        let mut moves_to_go: Option<u16> = None;
        let mut depth: Option<u16> = None;
        let mut nodes: Option<u64> = None;
        let mut mate: Option<u16> = None;
        let mut move_time: Option<Duration> = None;
        let mut infinite = false;

        let mut next_token_index = 1;
        loop {
            if tokens.len() <= next_token_index {
                break;
            }

            match tokens[next_token_index] {
                "ponder" => {
                    ponder = true;
                    next_token_index += 1;
                }
                "wtime" => {
                    white_time =
                        Some(Duration::from_millis(tokens[next_token_index + 1].parse::<u64>().map_err(|_| {
                            UciError::InvalidToken {
                                token: tokens[next_token_index + 1].to_string(),
                                expected: "wtime in milliseconds",
                            }
                        })?));
                    next_token_index += 2;
                }
                "btime" => {
                    black_time =
                        Some(Duration::from_millis(tokens[next_token_index + 1].parse::<u64>().map_err(|_| {
                            UciError::InvalidToken {
                                token: tokens[next_token_index + 1].to_string(),
                                expected: "btime in milliseconds",
                            }
                        })?));
                    next_token_index += 2;
                }
                "winc" => {
                    white_inc =
                        Some(Duration::from_millis(tokens[next_token_index + 1].parse::<u64>().map_err(|_| {
                            UciError::InvalidToken {
                                token: tokens[next_token_index + 1].to_string(),
                                expected: "winc in milliseconds",
                            }
                        })?));
                    next_token_index += 2;
                }
                "binc" => {
                    black_inc =
                        Some(Duration::from_millis(tokens[next_token_index + 1].parse::<u64>().map_err(|_| {
                            UciError::InvalidToken {
                                token: tokens[next_token_index + 1].to_string(),
                                expected: "binc in milliseconds",
                            }
                        })?));
                    next_token_index += 2;
                }
                "movestogo" => {
                    moves_to_go =
                        Some(tokens[next_token_index + 1].parse::<u16>().map_err(|_| UciError::InvalidToken {
                            token: tokens[next_token_index + 1].to_string(),
                            expected: "movestogo as an integer",
                        })?);
                    next_token_index += 2;
                }
                "searchmoves" => {
                    let moves = Self::read_search_moves(&tokens[next_token_index + 1..]);
                    next_token_index += moves.len() + 1;
                    search_moves = Some(moves);
                }
                "depth" => {
                    depth = Some(tokens[next_token_index + 1].parse::<u16>().map_err(|_| UciError::InvalidToken {
                        token: tokens[next_token_index + 1].to_string(),
                        expected: "depth as an integer",
                    })?);
                    next_token_index += 2;
                }
                "nodes" => {
                    nodes = Some(tokens[next_token_index + 1].parse::<u64>().map_err(|_| UciError::InvalidToken {
                        token: tokens[next_token_index + 1].to_string(),
                        expected: "nodes as an integer",
                    })?);
                    next_token_index += 2;
                }
                "mate" => {
                    mate = Some(tokens[next_token_index + 1].parse::<u16>().map_err(|_| UciError::InvalidToken {
                        token: tokens[next_token_index + 1].to_string(),
                        expected: "mate as an integer",
                    })?);
                    next_token_index += 2;
                }
                "movetime" => {
                    move_time =
                        Some(Duration::from_millis(tokens[next_token_index + 1].parse::<u64>().map_err(|_| {
                            UciError::InvalidToken {
                                token: tokens[next_token_index + 1].to_string(),
                                expected: "movetime in milliseconds",
                            }
                        })?));
                    next_token_index += 2;
                }
                "infinite" => {
                    infinite = true;
                    next_token_index += 1;
                }
                _ => {
                    return Err(UciError::InvalidToken {
                        token: tokens[next_token_index].to_string(),
                        expected: "ponder, wtime, binc, winc, movestogo, searchmoves, depth, nodes, mate, movetime or infinite",
                    });
                }
            }
        }

        self.engine.handle_go(
            search_moves,
            ponder,
            white_time,
            black_time,
            white_inc,
            black_inc,
            moves_to_go,
            depth,
            nodes,
            mate,
            move_time,
            infinite,
        )
    }

    /// Processes the "stop" command from the GUI to stop the engine search.
    ///
    /// # Returns
    /// `Ok(())` if the command was processed successfully, or an `UciError` if a problem occurred.
    fn handle_stop(&mut self) -> Result<(), UciError> {
        self.engine.handle_stop()
    }

    //==================================================================================================================
    // UCI commands from the engine to the GUI
    //==================================================================================================================

    /// Sends engine identification information to the GUI.
    ///
    /// This method outputs an identification string in UCI protocol format. It should be called during the
    /// initialization phase in response to the "uci" command.
    ///
    /// # Parameters
    /// * `id_type` - The type of identification (typically "name" or "author")
    /// * `value` - The value associated with the identification type
    ///
    /// # Example
    /// Common usage includes:
    /// - `send_id("name", "MyChessEngine")` - Identifies the engine name
    /// - `send_id("author", "Developer Name")` - Identifies the engine author
    pub fn send_id(id_type: &str, value: &str) {
        println!("id {} {}", id_type, value);
    }

    /// Sends an engine option definition to the GUI following the UCI protocol format.
    ///
    /// This function formats and outputs a UCI option definition based on the provided option type and name. The output
    /// strictly follows the UCI protocol standard for communicating available engine options to the GUI.
    ///
    /// # Parameters
    /// * `name` - The name of the option as it will appear in the GUI
    /// * `option_type` - The type and default values of the option as a `UciOptionType`
    pub fn send_option(name: &str, option_type: UciOptionType) {
        match option_type {
            UciOptionType::Check(default) => println!("option name {name} type check default {default}"),
            UciOptionType::Spin { default, min, max } => {
                println!("option name {name} type spin default {default} min {min} max {max}")
            }
            UciOptionType::Combo { default, values } => {
                let values_str = values.join(" var ");
                println!("option name {name} type combo default {default} var {values_str}")
            }
            UciOptionType::Button => println!("option name {name} type button"),
            UciOptionType::String(default) => println!(
                "option name {name} type string default {}",
                if default.is_empty() { String::from("<empty>") } else { default }
            ),
        }
    }

    /// Signals to the GUI that the engine has completed UCI initialization.
    ///
    /// This method outputs the "uciok" message, indicating that the engine has finished
    /// processing the "uci" command and has sent all identification and option information.
    /// It should be called after all engine identification and available options have been sent.
    ///
    /// The GUI will wait for this message before proceeding with further commands.
    pub fn send_uciok() {
        println!("uciok");
    }

    /// Signals to the GUI that the engine is ready for commands.
    ///
    /// This method outputs the "readyok" message in response to an "isready" command from the GUI.
    /// It indicates that the engine has completed any pending initialization tasks and is
    /// ready to accept and process further commands.
    ///
    /// The GUI typically waits for this response before sending commands that require
    /// the engine to be in a fully initialized state.
    pub fn send_readyok() {
        println!("readyok");
    }

    /// Notifies the GUI about an unrecognized command.
    ///
    /// This method outputs a message indicating that the engine received a command
    /// it doesn't understand or support.
    ///
    /// # Parameters
    /// * `command` - The unrecognized command string received from the GUI
    pub fn send_unknown_command(command: &str) {
        println!("Unknown command: {}", command);
    }

    /// Sends an informational string to the GUI.
    pub fn send_info_string(info: &str) {
        println!("info string {}", info);
    }

    /// Communicates the engine's best move choice to the GUI.
    ///
    /// This method outputs the best move found by the engine after a search operation. It must be sent after each "go"
    /// command (except in ponder mode) to indicate the engine's selected move. The message can optionally include a
    /// move that the engine predicts the opponent might play (ponder move).
    ///
    /// # Parameters
    /// * `best_move` - The best move found by the engine in UCI notation (e.g., "e2e4")
    /// * `ponder` - An optional move that the engine predicts the opponent might play next
    pub fn send_bestmove(best_move: &str, ponder: Option<&str>) {
        print!("bestmove {best_move}");
        if let Some(ponder) = ponder {
            print!(" ponder {ponder}");
        }
        println!();
    }

    /// Sends detailed search information to the GUI during calculation.
    ///
    /// This method outputs various search statistics and evaluation data in UCI protocol format. It allows the GUI to
    /// display the engine's analysis progress, current evaluation, principal variation, and other performance metrics
    /// during the search process.
    ///
    /// # Parameters
    /// * `options` - A `SendInfoOptions` structure containing the search information to be sent
    ///
    /// # Supported Information Fields
    /// * `depth` - Current search depth in plies
    /// * `sel_depth` - Selective search depth (deepest leaf nodes reached)
    /// * `time` - Time spent on the search so far
    /// * `nodes` - Number of nodes searched
    /// * `pv` - Principal variation (best line found)
    /// * `multi_pv` - Line number for multi-PV mode
    /// * `score` - Position evaluation in centipawns
    /// * `current_move` - Move currently being examined
    /// * `current_move_number` - Ordinal number of the current move
    /// * `hash_full` - Hash table usage percentage
    /// * `nps` - Search speed in nodes per second
    /// * `tb_hits` - Number of tablebase position lookups
    /// * `cpu_load` - CPU usage percentage
    /// * `string` - Arbitrary string information
    pub fn send_info(options: SendInfoOptions) {
        let mut cmd = String::new();

        cmd.push_str("info");

        if let Some(depth) = options.depth {
            cmd.push_str(&format!(" depth {}", depth.as_plies()));
        }
        if let Some(sel_depth) = options.sel_depth {
            cmd.push_str(&format!(" seldepth {}", sel_depth));
        }
        if let Some(time) = options.time {
            cmd.push_str(&format!(" time {}", time.as_millis()));
        }
        if let Some(nodes) = options.nodes {
            cmd.push_str(&format!(" nodes {}", nodes));
        }
        if let Some(pv) = options.pv {
            cmd.push_str(" pv");
            for move_str in pv {
                cmd.push_str(&format!(" {}", move_str));
            }
        }
        if let Some(multi_pv) = options.multi_pv {
            cmd.push_str(&format!(" multipv {}", multi_pv));
        }
        if let Some(score) = options.score {
            cmd.push_str(&format!(" score cp {}", score));
        }
        if let Some(current_move) = options.current_move {
            cmd.push_str(&format!(" currmove {}", current_move));
        }
        if let Some(current_move_number) = options.current_move_number {
            cmd.push_str(&format!(" currmovenumber {}", current_move_number));
        }
        if let Some(hash_full) = options.hash_full {
            cmd.push_str(&format!(" hashfull {}", hash_full));
        }
        if let Some(nps) = options.nps {
            cmd.push_str(&format!(" nps {}", nps));
        }
        if let Some(tb_hits) = options.tb_hits {
            cmd.push_str(&format!(" tbhits {}", tb_hits));
        }
        if let Some(cpu_load) = options.cpu_load {
            cmd.push_str(&format!(" cpuload {}", cpu_load));
        }
        if let Some(string) = options.string {
            cmd.push_str(&format!(" string {}", string));
        }

        println!("{}", cmd);
    }
}

/// Options for sending information to the UCI interface.
///
/// This struct follows the builder pattern using Rust's `Default` trait.
/// Fields are populated as needed and sent to the UCI interface via
/// the Uci::send_info method.
#[derive(Default)]
pub struct SendInfoOptions {
    // Current search depth in plies
    depth: Option<Depth>,

    /// Selective search depth - the deepest leaf nodes examined
    sel_depth: Option<u16>,

    /// Time spent searching so far
    time: Option<Duration>,

    /// Number of nodes searched so far
    nodes: Option<u64>,

    /// Principal variation - sequence of best moves found
    pv: Option<Vec<String>>,

    /// Index for multipv mode when searching multiple lines
    multi_pv: Option<u16>,

    /// Evaluation score in centipawns (positive for white advantage)
    score: Option<i16>,

    /// Move currently being examined
    current_move: Option<String>,

    /// Position of current_move in the move list being searched
    current_move_number: Option<u16>,

    /// Hash table usage in permille (0-1000)
    hash_full: Option<u16>,

    /// Search speed in nodes per second
    nps: Option<u64>,

    /// Number of tablebase position lookups
    tb_hits: Option<u64>,

    /// CPU load in permille (0-1000)
    cpu_load: Option<u16>,

    /// Arbitrary text information
    string: Option<String>,
}

/// A chess engine implementation of the Universal Chess Interface (UCI) protocol.
///
/// This struct maintains the current position state and manages the search process. It handles UCI commands from GUIs
/// and responds with appropriate UCI messages.
struct UciEngine {
    /// The current chess position being analyzed
    position: Position,

    /// The active search process (None when not searching)
    search: Option<Search>,

    /// The transposition table used for storing previously evaluated positions
    transposition_table: Arc<TranspositionTable>,
}

impl UciEngine {
    /// Creates a new UCI engine instance with default settings.
    ///
    /// Initializes the engine with a standard starting position and no active search. The engine is ready to receive
    /// UCI commands after creation.
    ///
    /// # Parameters
    /// * `tt_size` - The size of the transposition table in MB (must be a power of 2)
    ///
    /// # Returns
    /// A new `UciEngine` instance with the default chess starting position.
    fn new(tt_size: usize) -> Self {
        let transposition_table = Arc::new(TranspositionTable::new(tt_size * 1024 * 1024));
        UciEngine { position: Position::new(), search: None, transposition_table }
    }

    /// Processes and reports search progress information to the UCI interface.
    ///
    /// This function translates internal search progress data into UCI protocol messages. It handles different types of
    /// progress updates and formats them according to the UCI specification.
    ///
    /// # Parameters
    /// * `progress_type`: The type of progress update from the search algorithm
    ///
    /// # Progress Types
    /// * `Iteration`: Reports regular depth completion with current evaluation
    /// * `NewBestMove`: Reports when a new best move is found
    /// * `SearchFinished`: Reports the final best move when search completes
    /// * Other progress types are ignored
    fn report_progress(progress_type: ProgressType) {
        match progress_type {
            ProgressType::Iteration { depth, elapsed, score, nodes, pv } => {
                if elapsed > Duration::from_millis(100) {
                    let pv_as_string: Vec<String> = pv.iter().rev().map(|m| m.to_uci_string()).collect();
                    Uci::send_info(SendInfoOptions {
                        depth: Some(depth),
                        time: Some(elapsed),
                        nodes: Some(nodes),
                        pv: Some(pv_as_string),
                        score: Some(score.into()),
                        ..Default::default()
                    });
                }
            }
            ProgressType::NewBestMove { depth, elapsed, score, nodes, pv } => {
                if elapsed > Duration::from_millis(1000) {
                    let pv_as_string: Vec<String> = pv.iter().rev().map(|m| m.to_uci_string()).collect();
                    Uci::send_info(SendInfoOptions {
                        depth: Some(depth),
                        time: Some(elapsed),
                        nodes: Some(nodes),
                        pv: Some(pv_as_string),
                        score: Some(score.into()),
                        ..Default::default()
                    });
                }
            }
            ProgressType::SearchFinished { mv, elapsed: _, stats: _ } => {
                let mv_str = mv.to_uci_string();
                Uci::send_bestmove(&mv_str, None);
            }
            _ => {}
        }
    }

    //==================================================================================================================
    // UCI commands from the GUI to the engine
    //==================================================================================================================

    /// Handles the UCI 'uci' command from the GUI.
    ///
    /// Identifies the engine by sending its name and author information to the GUI, then signals protocol compatibility
    /// with 'uciok'.
    ///
    /// # Returns
    /// `Result<(), UciError>` - Ok if the command was processed successfully.
    fn handle_uci(&self) -> Result<(), UciError> {
        let options = Options::get();

        Uci::send_id("name", "Oxide9");
        Uci::send_id("author", "Mathieu Pagé <m@mathieupage.com>");
        Uci::send_option("Threads", UciOptionType::Spin { default: 1, min: 1, max: 1 });
        Uci::send_option(
            "Hash",
            UciOptionType::Spin { default: TranspositionTable::DEFAULT_MB_SIZE as u64, min: 1, max: 1024 * 1024 },
        );
        Uci::send_option(
            "moves_to_go_estimate",
            UciOptionType::Spin { default: options.moves_to_go_estimate() as u64, min: 5, max: 50 },
        );
        Uci::send_option(
            "max_time_ratio_per_move",
            UciOptionType::String(options.max_time_ratio_per_move().to_string()),
        );
        Uci::send_option("max_over_target_factor", UciOptionType::String(options.max_over_target_factor().to_string()));
        Uci::send_uciok();
        Ok(())
    }

    /// Generic method used by handle_setoption to parse and set an option value.
    fn set_option<T, F>(name: &str, text: &str, closure: F) -> Result<(), UciError>
    where
        T: std::str::FromStr,
        F: FnOnce(&mut Options, T),
    {
        let value = text
            .parse::<T>()
            .map_err(|_| UciError::InvalidOptionValue { option_name: name.to_string(), value: text.to_string() })?;
        Options::modify(|options| closure(options, value));
        Ok(())
    }

    /// Handles the UCI 'setoption' command from the GUI by applying the requested option change.
    ///
    /// This function processes a tparsed name-value pair from a UCI setoption command and updates the corresponding
    /// engine setting. Currently supported options include:
    ///
    /// - "Hash": Sets the transposition table size in MB
    ///
    /// # Parameters
    /// * `name` - The name of the option to set
    /// * `value` - The value to set for the option
    ///
    /// # Returns
    /// * `Ok(())` if the option was set successfully
    /// * `Err(UciError::InvalidHashSize)` if the Hash value couldn't be parsed to a valid number
    fn handle_setoption(&mut self, name: &str, text: &str) -> Result<(), UciError> {
        match name {
            "Hash" => {
                let size = text.parse::<usize>().map_err(|_| UciError::InvalidHashSize(text.to_string()))?;
                self.transposition_table = Arc::new(TranspositionTable::new(size * 1024 * 1024));
            }
            "moves_to_go_estimate" => {
                Self::set_option::<u32, _>(name, text, |options, value| {
                    options.set_moves_to_go_estimate(value);
                })?;
            }
            "max_time_ratio_per_move" => {
                Self::set_option::<f32, _>(name, text, |options, value| {
                    options.set_max_time_ratio_per_move(value);
                })?;
            }
            "max_over_target_factor" => {
                Self::set_option::<f32, _>(name, text, |options, value| {
                    options.set_max_over_target_factor(value);
                })?;
            }
            _ => {}
        }

        Ok(())
    }

    /// Handles the UCI 'isready' command from the GUI.
    ///
    /// Responds with 'readyok' to signal that the engine has completed initialization and is ready to receive and
    /// process commands.
    ///
    /// # Returns
    /// `Result<(), UciError>` - Ok if the command was processed successfully.
    fn handle_isready(&self) -> Result<(), UciError> {
        Uci::send_readyok();
        Ok(())
    }

    /// Handles the UCI 'position' command from the GUI.
    ///
    /// Sets up the internal board representation according to the specified FEN string and then applies any subsequent
    /// moves in sequence.
    ///
    /// # Parameters
    /// * `fen`: A Forsyth-Edwards Notation string describing the board position
    /// * `moves`: A list of moves in UCI coordinate notation (e.g., "e2e4") to apply after setting up the position
    ///
    /// # Returns
    /// `Result<(), UciError>` - Ok if the position was set up successfully, or an error if the FEN string or any of the
    /// moves are invalid.
    ///
    /// # Errors
    /// * `UciError::InvalidFenString` - If the provided FEN string cannot be parsed
    /// * `UciError::InvalidMove` - If any of the provided moves are illegal in the resulting position
    fn handle_position(&mut self, fen: String, moves: &[&str]) -> Result<(), UciError> {
        let mut position = Position::new_from_fen(&fen).map_err(|_| UciError::InvalidFenString(fen))?;
        for mv_str in moves {
            let mv =
                parse_coordinate_notation(&position, mv_str).map_err(|_| UciError::InvalidMove(mv_str.to_string()))?;
            position.make(Some(mv));
        }
        self.position = position;
        Ok(())
    }

    /// Handles the UCI 'go' command from the GUI to start a search.
    ///
    /// Configures a new search based on the specified time control parameters and initiates the search process on the
    /// current position.
    ///
    /// # Parameters
    /// * `search_moves`: Optional list of moves to restrict the search to
    /// * `ponder`: Whether the engine should ponder (think on opponent's time)
    /// * `white_time`: Remaining time for White in milliseconds
    /// * `black_time`: Remaining time for Black in milliseconds
    /// * `white_inc`: White's time increment per move in milliseconds
    /// * `black_inc`: Black's time increment per move in milliseconds
    /// * `moves_to_go`: Number of moves until next time control
    /// * `depth`: Maximum search depth in plies
    /// * `nodes`: Maximum number of nodes to search
    /// * `mate`: Search for mate in specified number of moves
    /// * `move_time`: Fixed time to search
    /// * `infinite`: Whether to search indefinitely until a 'stop' command
    ///
    /// # Returns
    /// `Result<(), UciError>` - Ok if the search was started successfully
    #[allow(unused_variables, clippy::too_many_arguments)]
    fn handle_go(
        &mut self,
        search_moves: Option<Vec<String>>,
        ponder: bool,
        white_time: Option<Duration>,
        black_time: Option<Duration>,
        white_inc: Option<Duration>,
        black_inc: Option<Duration>,
        moves_to_go: Option<u16>,
        depth: Option<u16>,
        nodes: Option<u64>,
        mate: Option<u16>,
        move_time: Option<Duration>,
        infinite: bool,
    ) -> Result<(), UciError> {
        let (time, inc) = match self.position.side_to_move() {
            Color::White => (white_time, white_inc),
            Color::Black => (black_time, black_inc),
        };
        let time_manager = TimeManager::new(TimeControl::new(time, inc, moves_to_go, move_time, infinite));

        self.search = Some(Search::new(
            self.position.clone(),
            Depth::from_plies(100),
            time_manager,
            Arc::new(move |progress: ProgressType| Self::report_progress(progress)),
            self.transposition_table.clone(),
            Arc::new(AtomicBool::new(false)),
        ));
        Ok(())
    }

    /// Handles the UCI 'stop' command from the GUI.
    ///
    /// This function attempts to stop any ongoing search operation. If a search is in progress, signals it to terminate
    /// as soon as possible. If no search is active, this function does nothing.
    ///
    /// # Returns
    /// `Ok(())` if the command was processed successfully, or an `UciError` if a problem occurred.
    fn handle_stop(&mut self) -> Result<(), UciError> {
        if let Some(search) = self.search.take() {
            search.stop();
        }
        Ok(())
    }
}
