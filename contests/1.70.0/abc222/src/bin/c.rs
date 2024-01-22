use std::collections::BTreeSet;

use proconio::{input, marker::Chars};

fn main() {
    input!(n: usize, m: usize, a: [Chars; 2 * n]);

    let mut players: BTreeSet<_> = (0..2 * n).map(|i| (n * m, i)).collect();
    for i in 0..m {
        let mut players_next = BTreeSet::new();
        for _ in 0..n {
            let p1 = players.pop_first().unwrap();
            let p2 = players.pop_first().unwrap();
            let (p1_cnt, p1_idx) = p1;
            let (p2_cnt, p2_idx) = p2;
            match (a[p1_idx][i], a[p2_idx][i]) {
                ('G', 'C') | ('C', 'P') | ('P', 'G') => {
                    players_next.insert((p1_cnt - 1, p1_idx));
                    players_next.insert(p2);
                },
                ('G', 'P') | ('C', 'G') | ('P', 'C') => {
                    players_next.insert(p1);
                    players_next.insert((p2_cnt - 1, p2_idx));
                },
                _ => {
                    players_next.insert(p1);
                    players_next.insert(p2);
                },
            }
        }
        players = players_next;
    }

    for (_, idx) in players {
        println!("{}", idx + 1);
    }
}
