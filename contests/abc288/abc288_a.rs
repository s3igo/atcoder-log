use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        ab: [(isize, isize); n],
    }

    println!("{}", ab.iter().map(|(a, b)| a + b).join("\n"));
}
