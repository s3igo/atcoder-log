use itertools::{iproduct, Itertools};
use proconio::input;

fn main() {
    input!(n: usize);

    let ans: Vec<_> = iproduct!(0..=n, 0..=n, 0..=n)
        .filter(|(i, j, k)| i + j + k <= n)
        .map(|(i, j, k)| format!("{i} {j} {k}"))
        .collect();

    println!("{}", ans.iter().join("\n"));
}
