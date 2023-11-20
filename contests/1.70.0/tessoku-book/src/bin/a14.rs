use itertools::{iproduct, Itertools};
use proconio::input;

fn main() {
    input!(n: usize, k: usize, a: [usize; n], b: [usize; n], c: [usize; n], d: [usize; n]);

    let q = iproduct!(c, d).map(|(c, d)| c + d).sorted().collect::<Vec<_>>();
    let cond = iproduct!(a, b).map(|(a, b)| a + b).any(|p| q.binary_search(&(k - p)).is_ok());

    println!("{}", if cond { "Yes" } else { "No" });
}
