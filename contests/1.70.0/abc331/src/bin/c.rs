use std::collections::{BTreeMap, HashMap};

use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n]);

    let map: BTreeMap<_, _> = a.iter().zip(1..).into_group_map().into_iter().collect();
    let map: HashMap<_, _> = map
        .iter()
        .rev()
        .scan(0, |acc, (&k, v)| {
            let prev = *acc;
            *acc += k * v.len();
            Some((k, prev))
        })
        .collect();

    let ans: Vec<_> = a.iter().map(|a| map[a]).collect();

    println!("{}", ans.iter().join(" "));
}
