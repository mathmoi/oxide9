use std::{path::PathBuf, sync::OnceLock};

use config::{Config, File};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("The configuration has already been initialized")]
    ConfigAlreadyInitialized,

    #[error("Unable to read the configuration file: {0}")]
    UnableToReadConfig(#[from] config::ConfigError),

    #[error("Config file not found")]
    ConfigFileNotFound,
}

/// Configuration for the engine
#[derive(Debug, serde::Deserialize)]
pub struct Oxide9Config {
    /// Number of threads to use for the perft command
    pub perft_threads: u32,

    /// Evaluation parameters for the engine
    pub eval: Eval,
}
/// Evaluation parameters for the engine
#[derive(Debug, serde::Deserialize)]
pub struct Eval {
    pub piece_type_game_phase: Vec<u8>,

    pub mg_piece_values: Vec<i16>,
    pub eg_piece_values: Vec<i16>,

    pub mg_pawn_table: Vec<i16>,
    pub eg_pawn_table: Vec<i16>,
    pub mg_knight_table: Vec<i16>,
    pub eg_knight_table: Vec<i16>,
    pub mg_bishop_table: Vec<i16>,
    pub eg_bishop_table: Vec<i16>,
    pub mg_rook_table: Vec<i16>,
    pub eg_rook_table: Vec<i16>,
    pub mg_queen_table: Vec<i16>,
    pub eg_queen_table: Vec<i16>,
    pub mg_king_table: Vec<i16>,
    pub eg_king_table: Vec<i16>,
}

static CONFIG: OnceLock<Oxide9Config> = OnceLock::new();

/// Get the configuration of the engine
pub fn get_config() -> &'static Oxide9Config {
    CONFIG.get().expect("The configuration should have been initialized")
}

fn get_config_path() -> Result<PathBuf, ConfigError> {
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

    Err(ConfigError::ConfigFileNotFound)
}

/// Initialize the configuration of the engine
pub fn initialize() -> Result<(), ConfigError> {
    let path = get_config_path()?;
    let settings = Config::builder().add_source(File::from(path.clone())).build()?;
    let config: Oxide9Config = settings.try_deserialize()?;
    CONFIG.set(config).expect("It should be possible to initialize the configuration");

    Ok(())
}
