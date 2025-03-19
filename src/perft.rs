use std::time::Instant;

use crate::{
    move_gen::{generation::generate_all_moves, move_list::MoveList},
    position::{FenError, Position},
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum PerftError {
    #[error("Invalid FEN ({}): {:?}", .0, .1)]
    InvalidFen(String, FenError),
}

// Execute a perft test on a given postion for a specified depth. The result will be printed to the console.
pub fn perft(fen: &str, depth: u32) -> Result<(), PerftError> {
    let mut position = Position::new_from_fen(fen).map_err(|e| PerftError::InvalidFen(fen.to_string(), e))?;

    println!("Perft ({}) for position:\n\n{}\n", depth, position.to_compact_string());

    let start = Instant::now();
    let nodes = divide(&mut position, depth);
    let duration = start.elapsed();

    println!("\nNodes: {}", nodes);
    println!("Time: {:.6}", duration.as_secs_f64());
    println!("Nodes per second: {:.6}", nodes as f64 / duration.as_secs_f64());

    Ok(())
}

fn divide(position: &mut Position, depth: u32) -> u64 {
    let mut moves = MoveList::new();
    generate_all_moves(position, &mut moves);
    let mut total = 0;
    for mv in moves.iter() {
        if position.is_legal(*mv) {
            position.make(*mv);
            let nodes = if depth == 1 { 1 } else { recursive_perft(position, depth - 1) };
            total += nodes;
            position.unmake();

            println!("{}\t{}", mv.to_uci_string(), nodes);
        }
    }
    total
}

fn recursive_perft(position: &mut Position, depth: u32) -> u64 {
    let mut nodes = 0;

    let mut moves = MoveList::new();
    generate_all_moves(position, &mut moves);

    if depth == 1 {
        nodes += moves.iter().filter(|m| position.is_legal(**m)).count() as u64;
    } else {
        for mv in moves.iter() {
            if position.is_legal(*mv) {
                position.make(*mv);
                nodes += recursive_perft(position, depth - 1);
                position.unmake();
            }
        }
    }
    nodes
}
