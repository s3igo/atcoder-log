use std::collections::HashSet;

use proconio::{input, marker::Usize1};

fn main() {
    input! {
        n: usize,
        m: usize,
    }

    let mut cnt = vec![HashSet::new(); n];
    for _ in 0..m {
        input! {
            k: usize,
            x: [Usize1; k],
        }

        for &i in &x {
            for &j in &x {
                cnt[i].insert(j);
            }
        }
    }

    let target = (0..n).collect::<HashSet<_>>();
    let cond = (0..n).all(|i| cnt[i] == target);

    println!("{}", if cond { "Yes" } else { "No" });
}
