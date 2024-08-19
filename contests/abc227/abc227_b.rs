use std::collections::HashSet;

use itertools::iproduct;
use proconio::input;

fn main() {
    input!(n: usize, s: [i32; n]);

    let results = iproduct!(1..=1000, 1..=1000)
        .filter_map(|(a, b)| match 3 * a + 4 * a * b + 3 * b {
            area @ ..=1000 => Some(area),
            _ => None,
        })
        .collect::<HashSet<_>>();

    let ans = s.iter().filter(|&s| !results.contains(s)).count();

    println!("{ans}");
}
