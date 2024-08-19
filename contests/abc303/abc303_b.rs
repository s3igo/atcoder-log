use std::collections::HashSet;

use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        m: usize,
        a: [[usize; n]; m],
    }

    // table[i] = (name, neighbors)
    let mut table = (1..=n).map(|x| (x, Vec::<usize>::new())).collect_vec();
    for e in a {
        for (&prev, &next) in e.iter().tuple_windows() {
            table[prev - 1].1.push(next);
            table[next - 1].1.push(prev);
        }
    }

    let ans = table.iter().fold(0, |acc, (i, e)| {
        let mut neighbors = e.iter().cloned().collect::<HashSet<_>>();
        neighbors.insert(*i);
        acc + (1..=n).collect::<HashSet<_>>().difference(&neighbors).count()
    }) / 2;

    println!("{}", ans);
}
