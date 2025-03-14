pub mod attacks; // TODO : Do we want to use pub mod instead of pub use, flattening the module structure?
mod generation;

pub use generation::{generate_moves, MoveGenerationType};
