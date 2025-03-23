use std::{
    sync::{Arc, RwLock},
    thread,
    time::Instant,
};

use crate::{
    move_gen::{generation::generate_all_moves, move_list::MoveList},
    position::{FenError, Position},
    r#move::Move,
};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum PerftError {
    #[error("Invalid FEN ({}): {:?}", .0, .1)]
    InvalidFen(String, FenError),
}

#[derive(Debug)]
enum PerftNodeStatus {
    New,
    Exclusive,
    Shared { children: Vec<(Move, PerftNode)> },
    Done { nodes: u64 },
}

#[derive(Debug, Clone)]
struct PerftNode {
    status: Arc<RwLock<PerftNodeStatus>>,
}

impl PerftNode {
    /// Create a new node with a new status.
    fn new() -> Self {
        Self { status: Arc::new(RwLock::new(PerftNodeStatus::New)) }
    }

    /// Returns the number of child for this node. If the node is not shared it has no childrent and the result is 0.
    fn child_count(&self) -> usize {
        let status = self.status.read().unwrap();

        if let PerftNodeStatus::Shared { children } = &*status {
            children.len()
        } else {
            0
        }
    }

    /// Returns the child at the specified index. If the node is not shared or the index is out of bounds, the result is None.
    fn get_child(&self, index: usize) -> Option<(Move, PerftNode)> {
        let status = self.status.read().unwrap();

        if let PerftNodeStatus::Shared { children } = &*status {
            if index < children.len() {
                Some((children[index].0, children[index].1.clone()))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Returns the number of nodes for this node. If the node is not done, the result is 0.
    fn get_nodes(&self) -> u64 {
        let status = self.status.read().unwrap();

        if let PerftNodeStatus::Done { nodes } = &*status {
            *nodes
        } else {
            0
        }
    }

    /// Returns true if the node is new.
    fn is_new(&self) -> bool {
        let status = self.status.read().unwrap();
        matches!(*status, PerftNodeStatus::New)
    }

    /// Returns true if the node is shared.
    fn is_shared(&self) -> bool {
        let status = self.status.read().unwrap();
        matches!(*status, PerftNodeStatus::Shared { .. })
    }

    fn is_done(&self) -> bool {
        let status = self.status.read().unwrap();
        matches!(*status, PerftNodeStatus::Done { .. })
    }

    /// Transform a new node into a shared node. If the node is not new, nothing happens.
    fn make_shared(&self, position: &Position) -> bool {
        let mut status = self.status.write().unwrap();

        if let PerftNodeStatus::New = *status {
            let mut moves = MoveList::new();
            generate_all_moves(&position, &mut moves);
            let children: Vec<(Move, PerftNode)> =
                moves.iter().filter(|mv| position.is_legal(**mv)).map(|mv| (*mv, PerftNode::new())).collect();
            *status = PerftNodeStatus::Shared { children };
            true
        } else {
            false
        }
    }

    /// Make a node exclusive. If the node is not new, nothing happens.
    fn make_exclusive(&self) -> bool {
        let mut status = self.status.write().unwrap();

        if let PerftNodeStatus::New = *status {
            *status = PerftNodeStatus::Exclusive;
            true
        } else {
            false
        }
    }

    /// Make an exclusive node done. This function should not be called if the node is not exclusive.
    fn make_done(&self, nodes: u64) {
        debug_assert!({ matches!(*self.status.read().unwrap(), PerftNodeStatus::Exclusive) });

        let mut status = self.status.write().unwrap();
        *status = PerftNodeStatus::Done { nodes };
    }

    /// Try to make a shared node done. If the node is not shared or all children are not done, nothing happens.
    fn try_make_done(&self) -> bool {
        let nodes;

        {
            let status = self.status.read().unwrap();
            if let PerftNodeStatus::Shared { children } = &*status {
                if children.iter().any(|(_, child)| !child.is_done()) {
                    return false;
                }

                nodes = children.iter().map(|(_, child)| child.get_nodes()).sum();
            } else {
                return false;
            }
        }

        let mut status = self.status.write().unwrap();
        if let PerftNodeStatus::Shared { .. } = &*status {
            *status = PerftNodeStatus::Done { nodes };
            true
        } else {
            false
        }
    }
}

// Execute a perft test on a given postion for a specified depth. The result will be printed to the console.
pub fn perft(fen: &str, depth: u32) -> Result<(), PerftError> {
    let mut position = Position::new_from_fen(fen).map_err(|e| PerftError::InvalidFen(fen.to_string(), e))?;

    println!("Perft ({}) for position:\n\n{}\n", depth, position.to_compact_string());

    let start = Instant::now();
    //let nodes = divide(&mut position, depth);
    let nodes = parallel_perft(&mut position, depth);
    let duration = start.elapsed();

    println!("\nNodes: {}", nodes);
    println!("Time: {:.6}", duration.as_secs_f64());
    println!("Nodes per second: {:.6}", nodes as f64 / duration.as_secs_f64());

    Ok(())
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

fn work_new_node(node: PerftNode, position: &mut Position, depth: u32, ply: u32, has_shared: bool) {
    const MAX_SHARED_DEPTH: u32 = 5;
    if has_shared || depth < MAX_SHARED_DEPTH {
        if !node.make_exclusive() {
            return;
        }

        let nodes = recursive_perft(position, depth);
        node.make_done(nodes);
        if ply == 1 {
            let mv = position.last_move().expect("There should always be a last move.");
            println!("{}\t{}", mv.to_uci_string(), nodes);
        }
    } else {
        if !node.make_shared(position) {
            return;
        }

        work_shared_node(node, position, depth, ply, true);
    }
}

fn work_shared_node(node: PerftNode, position: &mut Position, depth: u32, ply: u32, has_shared: bool) {
    // First we check for a new node to work on
    let child_count = node.child_count();
    for index in 0..child_count {
        let maybe_child = node.get_child(index);
        if let Some((mv, child)) = maybe_child {
            if child.is_new() {
                position.make(mv);
                work_new_node(child, position, depth - 1, ply + 1, has_shared);
                position.unmake();
            }
        } else {
            break;
        }
    }

    // Then we looks for shared nodes to help at
    loop {
        let mut helped = false;

        for index in 0..child_count {
            let maybe_child = node.get_child(index);
            if let Some((mv, child)) = maybe_child {
                if child.is_shared() {
                    helped = true;
                    position.make(mv);
                    work_shared_node(child, position, depth - 1, ply + 1, false);
                    position.unmake();
                }
            } else {
                break;
            }
        }

        if !helped {
            break;
        }
    }

    // Finally we check if we can make the node done
    if node.try_make_done() {
        if ply == 1 {
            let mv = position.last_move().expect("There should always be a last move at ply 1.");
            println!("{}\t{}", mv.to_uci_string(), node.get_nodes());
        }
    }
}

/// This is the worker function that will be executed by each thread.
fn parallel_perft_worker(node: PerftNode, position: &mut Position, depth: u32) {
    work_shared_node(node, position, depth, 0, false);
}

fn parallel_perft(position: &mut Position, depth: u32) -> u64 {
    let root = PerftNode::new();
    root.make_shared(position);

    // Spawning threads
    let mut threads = Vec::new();
    for _ in 0..32 {
        let thread_root = root.clone();
        let mut thread_position = position.clone();
        let handle = thread::spawn(move || {
            parallel_perft_worker(thread_root, &mut thread_position, depth);
        });
        threads.push(handle);
    }

    // Joins threads
    for handle in threads {
        handle.join().unwrap();
    }

    return root.get_nodes();
}
