use std::collections::HashSet;

use itertools::iproduct;
use proconio::input;

fn main() {
    input!(n: usize, s: [i32; n]);

    let f = |(i, j)| 3 * i + 4 * i * j + 3 * j;

    let results = iproduct!(1..=1000, 1..=1000)
        .filter(|&(a, b)| f((a, b)) <= 1000)
        .map(f)
        .collect::<HashSet<_>>();

    let ans = s.iter().filter(|&s| !results.contains(s)).count();

    println!("{ans}");
}
