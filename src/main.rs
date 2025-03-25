use clap::Parser;
use oxide9::{config::get_config, perft::perft};
use thiserror::Error;

#[derive(Error, Debug)]
enum Oxide9Error {
    #[error("Error during the perft command: {0}")]
    PerftError(#[from] oxide9::perft::PerftError),
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
            #[arg(short, long)]
            threads: Option<u32>,
        },
    }
}

fn run() -> Result<(), Oxide9Error> {
    // Parse command line arguments
    let args = arguments::Oxide9Args::parse();

    // Initialize the engine
    oxide9::initialize();
    let config = get_config();

    // Run the command
    match args.command.unwrap_or(arguments::Commands::Uci) {
        arguments::Commands::Uci => {
            unimplemented!();
        }
        arguments::Commands::Perft { depth, fen, threads } => {
            perft(&fen, depth, threads.unwrap_or(config.perft_threads))?;
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
