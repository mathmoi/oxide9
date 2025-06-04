use clap::Parser;
use oxide9::{analyze::analyze, bench::bench, depth::Depth, perft::perft, uci::run_uci};
use thiserror::Error;

#[derive(Error, Debug)]
enum Oxide9Error {
    #[error("Error during the perft command: {0}")]
    PerftError(#[from] oxide9::perft::PerftError),

    #[error("Error during the analysis command: {0}")]
    AnalyzeError(#[from] oxide9::analyze::AnalyzeError),
}

mod arguments {
    use clap::{Parser, Subcommand};

    /// A chess engine written by Mathieu Pagé
    #[derive(Parser)]
    #[command(
        name = "oxide9",
        author = "Mathieu Pagé",
        version = "0.1.0",
        about = "A chess engine written by Mathieu Pagé",
        subcommand_negates_reqs = true // This allows the user to run the program without any subcommands
    )]
    pub struct Oxide9Args {
        #[command(subcommand)]
        pub command: Option<Commands>,
    }

    #[derive(Debug, Clone, Subcommand)]
    pub enum Commands {
        /// Start the UCI protocol (default commnand)
        Uci {
            /// The size of the transposition table in megabytes (this must be a power of 2)
            #[arg(long, default_value = "128")]
            tt_size: usize,
        },

        /// Calculate the perft of a position
        Perft {
            /// The depth to calculate the perft
            #[arg(short, long)]
            depth: u16,

            /// FEN string representing the position to calculate the perft
            #[arg(short, long, default_value = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")]
            fen: String,

            /// The number of threads to use for the perft calculation
            #[arg(short, long, default_value = "1")]
            threads: usize,
        },

        /// Analyze a position
        Analyze {
            /// FEN string representing the position to analyze
            #[arg(short, long, default_value = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")]
            fen: String,

            /// The depth to analyze the position to
            #[arg(short, long, default_value = "100")]
            depth: u16,

            /// The size of the transposition table in megabytes (this must be a power of 2)
            #[arg(long, default_value = "128")]
            tt_size: usize,
        },

        /// Run a benchmark
        Bench,
    }
}

fn run() -> Result<(), Oxide9Error> {
    // Initialize the engine
    oxide9::initialize();

    // Check if the command is ommitted. If so we run the UCI command by default.
    let mut args: Vec<String> = std::env::args().collect();
    if args.len() <= 1 || args[1].starts_with('-') {
        args.insert(1, "uci".to_string());
    }
    let cli_args = arguments::Oxide9Args::parse_from(args);

    // Run the command
    match cli_args.command {
        Some(arguments::Commands::Uci { tt_size }) => {
            run_uci(tt_size);
        }
        Some(arguments::Commands::Perft { depth, fen, threads }) => {
            perft(&fen, depth, threads)?;
        }
        Some(arguments::Commands::Analyze { fen, depth, tt_size }) => {
            analyze(&fen, Depth::from_plies(depth as i16), tt_size)?;
        }
        Some(arguments::Commands::Bench) => {
            bench();
        }
        None => {
            unreachable!("This case should never happen because we check for it at the beginning of the function");
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
