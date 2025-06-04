use std::{
    sync::{atomic::AtomicBool, Arc, Mutex},
    time::Duration,
};

use crate::{
    depth::Depth,
    position::Position,
    search::{ProgressType, Search},
    time::{TimeControl, TimeManager},
    tt::TranspositionTable,
};

const BENCH_POSITIONS: [(&str, Depth); 12] = [
    ("r1bqrbk1/1pp2pp1/p1np1n1p/4p3/B3P2B/2PP1N2/PP1N1PPP/R2QR1K1 b - - 1 11", Depth::from_plies(9)),
    ("r2q1rk1/4bppp/p1pp1n2/1p2p3/3PP3/PPN1B2P/1P3PP1/R2Q1RK1 b - - 1 14", Depth::from_plies(9)),
    ("8/6p1/5bk1/p4p1p/r5P1/5K1P/P1R2P2/2B5 w - - 0 37", Depth::from_plies(12)),
    ("1r2r1k1/1b3p1p/p1p3p1/2p1q2n/P1N1P3/7P/2Q2PP1/3RRBK1 b - - 1 26", Depth::from_plies(10)),
    ("7k/4R3/3p1r2/4p2p/4P3/1Q3N2/4KPq1/8 b - - 3 45", Depth::from_plies(10)),
    ("2rqr1k1/4bpp1/p2p1n1p/1p6/3QP3/1b4NP/PP3PP1/R1B1R1K1 w - - 0 20", Depth::from_plies(10)),
    ("3k4/8/2RK4/6r1/3P4/8/8/8 b - - 14 60", Depth::from_plies(18)),
    ("r2r2k1/4bp2/6pp/pBp1p3/P7/2P1P2P/5KP1/2RR4 w - - 0 32", Depth::from_plies(11)),
    ("r1bq1rk1/2p1bppp/p1n2n2/1p1pp3/4P3/1BP2N2/PP1P1PPP/RNBQR1K1 w - - 0 9", Depth::from_plies(12)),
    ("6k1/4bppp/8/2P1P3/1p3B2/1B1b3P/5PP1/6K1 b - - 0 35", Depth::from_plies(12)),
    ("r2q2k1/1p3r2/p2p2pp/3Ppn2/4N3/2R4P/PP1Q1PP1/2R3K1 w - - 0 23", Depth::from_plies(8)),
    ("r5k1/5ppp/p1R5/1p1PN2b/1Q6/1r5P/1P4P1/5RK1 b - - 0 27", Depth::from_plies(12)),
];

/// Run a benchmark of the speed of the search algorithm.
pub fn bench() {
    let sum = Arc::new(Mutex::new((0_u64, Duration::from_secs(0))));
    let sum_clone = Arc::clone(&sum);

    let closure = Arc::new(move |progress: ProgressType| {
        if let ProgressType::SearchFinished { mv: _, elapsed, stats } = progress {
            let mut sum_guard = sum_clone.lock().unwrap();
            sum_guard.0 += stats.total_nodes;
            sum_guard.1 += elapsed;
        }
    });

    for (fen, depth) in BENCH_POSITIONS {
        const TT_SIZE: usize = 16 * 1024 * 1024;
        let position = Position::new_from_fen(fen).expect("FEN string should be valid");
        let transposition_table = Arc::new(TranspositionTable::new(TT_SIZE));
        let cancelation_token = Arc::new(AtomicBool::new(false));
        let handle = Search::new(
            position,
            depth,
            TimeManager::new(TimeControl::Infinite),
            closure.clone(),
            transposition_table,
            cancelation_token,
        );
        handle.join();
    }

    let sum_guard = sum.lock().unwrap();
    println!("{} nodes {:.0} nps", sum_guard.0, sum_guard.0 as f64 / sum_guard.1.as_secs_f64());
}
