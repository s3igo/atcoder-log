use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        s: [String; n],
    }

    println!("{}", s.iter().rev().join("\n"));
}
