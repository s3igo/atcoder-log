use std::collections::HashSet;

use itertools::Itertools;
use proconio::input;

fn main() {
    input!(cards: [usize; 5]);

    let dict = cards.iter().counts();
    let cond = dict.into_values().collect::<HashSet<_>>() == HashSet::from([2, 3]);

    println!("{}", if cond { "Yes" } else { "No" });
}
