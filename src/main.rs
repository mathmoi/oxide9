use std::path::PathBuf;

use clap::Parser;
use oxide9::{
    config::{self, get_config},
    perft::perft,
};
use thiserror::Error;

#[derive(Error, Debug)]
enum Oxide9Error {
    #[error("Error during the perft command: {0}")]
    PerftError(#[from] oxide9::perft::PerftError),

    #[error("Config file not found")]
    ConfigFileNotFound,

    #[error("Error reading the configuration file: {0}")]
    ConfigError(#[from] oxide9::config::ConfigError),
}

mod arguments {
    use clap::{Parser, Subcommand};
    use std::path::{Path, PathBuf};

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

fn get_config_path() -> Result<PathBuf, Oxide9Error> {
    let config_filename = "oxide9.toml";

    // Check several possible locations for config file
    let paths = vec![
        // Current directory
        PathBuf::from(config_filename),
        // Assets directory
        PathBuf::from("assets/config").join(config_filename),
    ];

    // Return the first path that exists
    for path in &paths {
        if path.exists() {
            return Ok(path.clone());
        }
    }

    Err(Oxide9Error::ConfigFileNotFound)
}

fn run() -> Result<(), Oxide9Error> {
    // Read configuration file
    let config_path = get_config_path()?;
    oxide9::config::initialize(config_path)?;
    let config = get_config();

    // Parse command line arguments
    let args = arguments::Oxide9Args::parse();

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
