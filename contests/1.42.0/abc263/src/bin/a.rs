use std::collections::HashSet;

use itertools::Itertools;
use proconio::input;

fn main() {
    input!(cards: [usize; 5]);

    let cnt = cards.into_iter().counts();
    let set = cnt.into_values().collect::<HashSet<_>>();
    let cond = set == HashSet::from([2, 3]);

    println!("{}", if cond { "Yes" } else { "No" });
}
