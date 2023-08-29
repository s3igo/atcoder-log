use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        x: [usize; 5 * n],
    }

    let ans = x.iter().sorted().skip(n).rev().skip(n).sum::<usize>() as f64 / (3 * n) as f64;

    println!("{}", ans);
}
