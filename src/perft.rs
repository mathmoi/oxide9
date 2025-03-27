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

/// Represents errors that can occur during perft (performance test) operations.
///
/// This enum encompasses all possible errors that might be encountered when performing perft calculations on chess
/// positions.
///
/// # Variants
/// * `InvalidFen(String, FenError)` - Indicates that the provided FEN string was invalid, including both the original
///   FEN string and the specific parsing error
#[derive(Error, Debug)]
pub enum PerftError {
    #[error("Invalid FEN ({}): {:?}", .0, .1)]
    InvalidFen(String, FenError),
}

/// Represents the possible states of a node in the parallel perft tree.
///
/// This enum tracks the processing status of a position during parallel perft execution, facilitating work distribution
/// and result aggregation across multiple threads.
///
/// # Variants
/// * `New` - A newly created node that hasn't been claimed by any thread
/// * `Exclusive` - A node being processed exclusively by a single thread
/// * `Shared { children }` - A node being processed cooperatively by multiple threads, containing its legal moves and
///   corresponding child nodes
/// * `Done { nodes }` - A completed node with its final node count
///
/// # Note
/// The state transitions (New → Exclusive/Shared → Done) are designed to minimize contention while enabling efficient
/// parallel execution.
#[derive(Debug)]
enum PerftNodeStatus {
    New,
    Exclusive,
    Shared { children: Vec<(Move, PerftNode)> },
    Done { nodes: u64 },
}

/// A thread-safe node in the parallel perft calculation tree.
///
/// This structure represents a position in the perft tree and manages its processing state across multiple threads. It
/// uses atomic operations and locks to coordinate work distribution and result collection in a thread-safe manner.
///
/// # Fields
/// * `status` - A thread-safe reference to the node's current processing status, implemented as an atomic
///   reference-counted read-write lock for efficient concurrent access.
///
/// # Note
/// This structure is designed to be safely cloned and shared between threads during parallel perft computation, with
/// the internal state protected by appropriate synchronization primitives.
#[derive(Debug, Clone)]
struct PerftNode {
    status: Arc<RwLock<PerftNodeStatus>>,
}

impl PerftNode {
    /// Creates a new perft node in the initial `New` state.
    ///
    /// Instantiates a fresh node for the parallel perft computation tree with its processing status set to `New`. Nodes
    /// in this state are available to be claimed by any worker thread.
    ///
    /// # Returns
    /// * A new `PerftNode` instance ready to be used in the parallel computation
    fn new() -> Self {
        Self { status: Arc::new(RwLock::new(PerftNodeStatus::New)) }
    }

    /// Returns the number of children for this node.
    ///
    /// Retrieves the count of legal moves (child nodes) from this position. Only shared nodes have children; all other
    /// node states return 0.
    ///
    /// # Returns
    /// * The number of child nodes if the node is in the `Shared` state
    /// * 0 if the node is in any other state (`New`, `Exclusive`, or `Done`)
    fn child_count(&self) -> usize {
        let status = self.status.read().unwrap();

        if let PerftNodeStatus::Shared { children } = &*status {
            children.len()
        } else {
            0
        }
    }

    /// Returns the child node at the specified index.
    ///
    /// Retrieves a specific move and its corresponding node from this position's children. This method only returns
    /// values for nodes in the `Shared` state.
    ///
    /// # Parameters
    /// * `index` - The zero-based index of the child to retrieve
    ///
    /// # Returns
    /// * `Some((move, node))` - The move and corresponding perft node if found
    /// * `None` - If the node is not in the `Shared` state or the index is out of bounds
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

    /// Returns the total number of leaf nodes counted beneath this node.
    ///
    /// Retrieves the final node count for a completed perft calculation. This method only returns a non-zero value when
    /// the node is in the `Done` state.
    ///
    /// # Returns
    /// * The total number of positions counted if the node has completed processing
    /// * 0 if the node is still in progress (in `New`, `Exclusive`, or `Shared` state)
    fn get_nodes(&self) -> u64 {
        let status = self.status.read().unwrap();

        if let PerftNodeStatus::Done { nodes } = &*status {
            *nodes
        } else {
            0
        }
    }

    /// Returns whether the node is in the `New` state.
    ///
    /// Checks if this node has not yet been claimed by any thread for processing. New nodes are available to be
    /// transitioned to either `Exclusive` or `Shared` state.
    ///
    /// # Returns
    /// * `true` if the node is in the `New` state
    /// * `false` if the node is in any other state
    fn is_new(&self) -> bool {
        let status = self.status.read().unwrap();
        matches!(*status, PerftNodeStatus::New)
    }

    /// Returns whether the node is in the `Shared` state.
    ///
    /// Determines if this node is currently being processed cooperatively by multiple threads. Shared nodes contain
    /// child nodes that can be worked on concurrently.
    ///
    /// # Returns
    /// * `true` if the node is in the `Shared` state with child nodes
    /// * `false` if the node is in any other state
    fn is_shared(&self) -> bool {
        let status = self.status.read().unwrap();
        matches!(*status, PerftNodeStatus::Shared { .. })
    }

    /// Returns whether the node is in the `Done` state.
    ///
    /// Checks if this node has completed its perft calculation and has a final node count. Done nodes have finished
    /// processing and their results are available.
    ///
    /// # Returns
    /// * `true` if the node has completed processing and contains a final node count
    /// * `false` if the node is still in progress
    fn is_done(&self) -> bool {
        let status = self.status.read().unwrap();
        matches!(*status, PerftNodeStatus::Done { .. })
    }

    /// Transforms a new node into a shared node for parallel processing.
    ///
    /// Attempts to transition this node from the `New` state to the `Shared` state by generating all legal moves from
    /// the current position and creating child nodes. Only succeeds if the node is currently in the `New` state.
    ///
    /// # Parameters
    /// * `position` - The chess position to generate moves from
    ///
    /// # Returns
    /// * `true` if the node was successfully transformed to `Shared` state
    /// * `false` if the node was already in another state
    ///
    /// # Note
    /// When successful, this creates child nodes for all legal moves from the position, enabling parallel distribution
    /// of the workload.
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

    /// Claims a node for exclusive processing by a single thread.
    ///
    /// Attempts to transition this node from the `New` state to the `Exclusive` state, indicating that a single thread
    /// will process this position completely. Only succeeds if the node is currently in the `New` state.
    ///
    /// # Returns
    /// * `true` if the node was successfully claimed for exclusive processing
    /// * `false` if the node was already in another state
    ///
    /// # Note
    /// When claiming a node exclusively, the thread commits to processing the entire subtree beneath this position
    /// without sharing the workload.
    fn make_exclusive(&self) -> bool {
        let mut status = self.status.write().unwrap();

        if let PerftNodeStatus::New = *status {
            *status = PerftNodeStatus::Exclusive;
            true
        } else {
            false
        }
    }

    /// Marks an exclusive node as complete with its final node count.
    ///
    /// Transitions the node from `Exclusive` state to `Done` state, storing the total number of leaf nodes calculated
    /// beneath this position.
    ///
    /// # Parameters
    /// * `nodes` - The total number of leaf nodes counted for this position
    fn make_done(&self, nodes: u64) {
        debug_assert!({ matches!(*self.status.read().unwrap(), PerftNodeStatus::Exclusive) });

        let mut status = self.status.write().unwrap();
        *status = PerftNodeStatus::Done { nodes };
    }

    /// Attempts to complete a shared node by aggregating its children's results.
    ///
    /// Tries to transition this node from the `Shared` state to the `Done` state by:
    /// 1. Verifying all child nodes have completed their calculations
    /// 2. Summing the node counts from all children
    /// 3. Storing the total as this node's final result
    ///
    /// # Returns
    /// * `true` if the node was successfully marked as done
    /// * `false` if the node was not in the `Shared` state or has incomplete children
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

/// Execute a perft (performance test) on a given chess position for a specified depth.
///
/// Perft tests count the number of leaf nodes in a move generation tree of a specified depth. This function performs
/// the test and prints the results to the console, including the total node count, execution time, and nodes processed
/// per second.
///
/// # Parameters
/// * `fen` - A FEN string representation of the chess position to analyze
/// * `depth` - The depth of the move tree to traverse
/// * `threads` - Number of threads to use for parallel computation
///
/// # Returns
/// * `Ok(())` - If the perft test completes successfully
/// * `Err(PerftError)` - If an error occurs, such as an invalid FEN string
///
/// # Note
/// The function automatically switches to parallel computation for depths greater than 5 to improve performance on
/// multi-core systems.
pub fn perft(fen: &str, depth: u16, threads: u32) -> Result<(), PerftError> {
    const MIN_PARALLEL_DEPTH: u16 = 5;

    let mut position = Position::new_from_fen(fen).map_err(|e| PerftError::InvalidFen(fen.to_string(), e))?;

    println!("Perft ({}) for position:\n\n{}\n", depth, position.to_compact_string());

    let start = Instant::now();
    let nodes = if depth <= MIN_PARALLEL_DEPTH {
        divide(&mut position, depth)
    } else {
        parallel_perft(&mut position, depth, threads)
    };
    let duration = start.elapsed();

    println!("\nNodes: {}", nodes);
    println!("Time: {:.3}", duration.as_secs_f64());
    println!("Nodes per second: {:.0}", nodes as f64 / duration.as_secs_f64());

    Ok(())
}

/// Generates a detailed "divide" view of the perft (performance test) results.
///
/// This function performs a perft test, breaking down the results by each possible first move in the position. For each
/// legal move, it displays the move in UCI notation and the number of leaf nodes found beneath that move.
///
/// # Parameters
/// * `position` - A mutable reference to the chess position to analyze
/// * `depth` - The depth of the move tree to traverse
///
/// # Returns
/// * The total number of leaf nodes found across all moves
///
/// # Note
/// This function prints the results for each move directly to the console, making it useful for debugging move
/// generation or comparing against other chess engines.
fn divide(position: &mut Position, depth: u16) -> u64 {
    let mut total_nodes = 0;

    let mut moves = MoveList::new();
    generate_all_moves(position, &mut moves);

    for mv in moves.iter() {
        if position.is_legal(*mv) {
            let nodes;
            if depth == 1 {
                nodes = 1;
            } else {
                position.make(*mv);
                nodes = recursive_perft(position, depth - 1);
                position.unmake();
            };
            println!("{}\t{}", mv.to_uci_string(), nodes);
            total_nodes += nodes;
        }
    }

    total_nodes
}

/// Recursively calculates the number of leaf nodes in a move tree at a specified depth.
///
/// This is an internal helper function for perft testing that counts positions at the specified depth. It optimizes the
/// counting process by using special handling for the terminal depth level.
///
/// # Parameters
/// * `position` - A mutable reference to the chess position to analyze
/// * `depth` - The remaining depth to traverse in the move tree
///
/// # Returns
/// * The total number of legal positions found at the specified depth
///
/// # Note
/// At depth 1, the function avoids making/unmaking moves and simply counts legal moves. For greater depths, it
/// recursively explores each legal move.
fn recursive_perft(position: &mut Position, depth: u16) -> u64 {
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

/// Performs a parallel perft test using multiple threads to improve performance.
///
/// This function distributes the perft workload across multiple threads by sharing a tree of positions to be analyzed.
/// Each thread dynamically processes nodes from this shared work tree until all positions at the specified depth have
/// been counted.
///
/// # Parameters
/// * `position` - A mutable reference to the starting chess position
/// * `depth` - The depth of the move tree to traverse
/// * `threads_count` - The number of worker threads to create
///
/// # Returns
/// * The total number of leaf nodes found at the specified depth
///
/// # Note
/// This function creates a shared work distribution system to effectively balance the workload across all available
/// threads, which significantly improves performance for deeper perft tests on multi-core systems.
fn parallel_perft(position: &mut Position, depth: u16, threads_count: u32) -> u64 {
    let root = PerftNode::new();
    root.make_shared(position);

    // Spawning threads
    let mut threads = Vec::with_capacity(threads_count as usize);
    for _ in 0..threads_count {
        let thread_root = root.clone();
        let mut thread_position = position.clone();
        let handle = thread::spawn(move || {
            work_shared_node(thread_root, &mut thread_position, depth, 0, false);
        });
        threads.push(handle);
    }

    // Joins threads
    for handle in threads {
        handle.join().unwrap();
    }

    return root.get_nodes();
}

/// Processes a shared node in the parallel perft work distribution system.
///
/// This function implements the worker thread logic for shared perft execution, focusing on efficient workload
/// distribution and minimal thread contention.
///
/// # Parameters
/// * `node` - The shared perft node to process
/// * `position` - A mutable reference to the current chess position
/// * `depth` - The remaining depth to analyze
/// * `ply` - The current ply (half-move) depth in the search
/// * `has_shared` - Whether the thread has already shared work
///
/// # Operation
/// The function follows a three-phase approach:
/// 1. First processes any new, unclaimed nodes
/// 2. Then helps with already shared nodes
/// 3. Finally attempts to mark the node as completed
///
/// # Note
/// At ply 1, the function prints move-specific results to maintain compatibility with the divide view's output format.
fn work_shared_node(node: PerftNode, position: &mut Position, depth: u16, ply: u32, has_shared: bool) {
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

/// Processes a new node in the parallel perft work distribution system.
///
/// This function handles newly discovered nodes during parallel perft calculation, either processing them directly or
/// sharing them among threads depending on depth.
///
/// # Parameters
/// * `node` - The perft node to process
/// * `position` - A mutable reference to the current chess position
/// * `depth` - The remaining depth to analyze
/// * `ply` - The current ply (half-move) depth in the search
/// * `has_shared` - Whether the thread has already shared work
///
/// # Operation
/// The function makes a strategic decision based on depth and sharing status:
/// - For shallow depths (< 5) or when already sharing work: processes the node exclusively
/// - For deeper positions: shares the node for parallel processing
///
/// # Note
/// The depth threshold for sharing (MAX_SHARED_DEPTH = 5) balances the overhead of work distribution against the
/// benefits of parallelism.
fn work_new_node(node: PerftNode, position: &mut Position, depth: u16, ply: u32, has_shared: bool) {
    const MAX_SHARED_DEPTH: u16 = 5;
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
