use clap::Parser;
use oxide9::{analyze::analyze, perft::perft, uci::run_uci};
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

        /// Indicate if output number should be precise instead of human-readable
        #[arg(short, long)]
        pub precise: bool,

        /// The number of threads to use for the perft calculation
        #[arg(short, long)]
        pub threads: Option<u32>,

        /// The size of the transposition table in megabytes (this must be a power of 2)
        #[arg(short, long)]
        pub tt_size: Option<u32>,
    }

    #[derive(Debug, Clone, Subcommand)]
    pub enum Commands {
        /// Start the UCI protocol (default commnand)
        Uci,

        /// Calculate the perft of a position
        Perft {
            /// The depth to calculate the perft
            #[arg(short, long)]
            depth: u16,

            /// FEN string representing the position to calculate the perft
            #[arg(short, long, default_value = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")]
            fen: String,
        },

        /// Analyze a position
        Analyze {
            /// FEN string representing the position to analyze
            #[arg(short, long, default_value = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")]
            fen: String,

            /// The depth to analyze the position to
            #[arg(short, long)]
            depth: u16,
        },
    }
}

fn run() -> Result<(), Oxide9Error> {
    // Parse command line arguments
    let args = arguments::Oxide9Args::parse();

    // Initialize the engine
    oxide9::initialize_with_args(args.threads, args.tt_size, args.precise);

    // Run the command
    match args.command.unwrap_or(arguments::Commands::Uci) {
        arguments::Commands::Uci => {
            run_uci();
        }
        arguments::Commands::Perft { depth, fen } => {
            perft(&fen, depth)?;
        }
        arguments::Commands::Analyze { fen, depth } => {
            analyze(&fen, depth)?;
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
