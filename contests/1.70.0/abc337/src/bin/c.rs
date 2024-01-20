use std::collections::HashMap;

use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, a: [isize; n]);

    let map: HashMap<_, _> = a.iter().zip(1..).collect();
    let mut ans = vec![map[&-1]];
    for _ in 0..n - 1 {
        ans.push(map[ans.last().unwrap()]);
    }

    println!("{}", ans.iter().join(" "));
}
