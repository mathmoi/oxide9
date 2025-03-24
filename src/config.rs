use std::{path::PathBuf, sync::OnceLock};

use config::{Config, File};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("The configuration has already been initialized")]
    ConfigAlreadyInitialized,

    #[error("Unable to read the configuration file: {0}")]
    UnableToReadConfig(#[from] config::ConfigError),
}

/// Configuration for the engine
#[derive(Debug, serde::Deserialize)]
pub struct Oxide9Config {
    /// Number of threads to use for the perft command
    pub perft_threads: u32,
}

static CONFIG: OnceLock<Oxide9Config> = OnceLock::new();

/// Get the configuration of the engine
pub fn get_config() -> &'static Oxide9Config {
    CONFIG.get().expect("The configuration should have been initialized")
}

/// Initialize the configuration of the engine
pub fn initialize(path: PathBuf) -> Result<(), ConfigError> {
    let settings = Config::builder().add_source(File::from(path.clone())).build()?;
    let config: Oxide9Config = settings.try_deserialize()?;
    CONFIG.set(config).expect("It should be possible to initialize the configuration");

    Ok(())
}
