use std::collections::HashSet;

use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, k: usize, a: [usize; n], b: [usize; k]);

    let max_set = a.iter().zip(1..).max_set_by_key(|(&a, _)| a);
    let candidates = max_set.iter().map(|(_, i)| i).collect::<HashSet<_>>();

    let cond = !b.iter().collect::<HashSet<_>>().is_disjoint(&candidates);

    println!("{}", if cond { "Yes" } else { "No" });
}
