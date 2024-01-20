use std::collections::HashMap;

use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, a: [isize; n]);

    let map: HashMap<_, _> = a.iter().zip(1..).sorted().collect();
    let mut ans: Vec<isize> = vec![map[&-1]];
    for _ in 0..n - 1 {
        let next = map[ans.last().unwrap()];
        ans.push(next);
    }

    println!("{}", ans.iter().join(" "));
}
