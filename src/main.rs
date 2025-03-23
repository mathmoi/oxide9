use clap::{Parser, Subcommand};
use oxide9::perft::perft;
use thiserror::Error;

#[derive(Error, Debug)]
enum Oxide9Error {
    #[error("Error during the perft command: {0}")]
    PerftError(#[from] oxide9::perft::PerftError),
}

/// Command-line interface arguments for the oxide9 chess engine.
///
/// This struct defines the top-level CLI arguments and subcommands accepted by the chess engine application.
///
/// # Structure
/// The application supports multiple commands through subcommands while making these subcommands optional (via
/// subcommand_negates_reqs).
///
/// # Fields
/// * `command` - An optional subcommand to execute. When not provided, the application will run the `uci` command
#[derive(Parser)]
#[command(
    name = "oxide9",
    author = "Mathieu Pagé",
    version = "0.1.0",
    about = "A chess engine written by Mathieu Pagé",
    subcommand_negates_reqs = true // This allow subcommands to be optional
)]

/// Commands supported by the oxide9 chess engine.
///
/// This enum defines the available subcommands that can be executed by the application. Each variant represents a
/// different operation mode for the engine.
///
/// # Commands
/// * `Uci` - Starts the engine in UCI (Universal Chess Interface) protocol mode. This is the default command when no
///          subcommand is specified.
///
/// * `Perft` - Performs a performance test (node counting) on a chess position. This command is used for debugging and
///           verification of the move generator. - `depth`: The search depth for the perft calculation - `fen`: Chess
///           position in FEN notation to analyze. Defaults to the standard starting position if not specified.
struct Oxide9Args {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Debug, Clone, Subcommand)]
enum Commands {
    /// Start the UCI protocol (default commnand)
    Uci,

    /// Calculate the perft of a position
    Perft {
        /// The depth to calculate the perft
        #[arg(short, long)]
        depth: u32,

        /// FEN string representing the position to calculate the perft
        #[arg(short, long, default_value = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")]
        fen: String,

        /// The number of threads to use for the perft calculation
        #[arg(short, long, default_value = "16")]
        threads: u32,
    },
}

fn run() -> Result<(), Oxide9Error> {
    oxide9::initialize();

    let args = Oxide9Args::parse();

    match args.command.unwrap_or(Commands::Uci) {
        Commands::Uci => {
            unimplemented!();
        }
        Commands::Perft { depth, fen, threads } => {
            perft(&fen, depth, threads)?;
        }
    }
    Ok(())
}

/// Main entry point for the oxide9 chess engine.
fn main() {
    if let Err(e) = run() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
