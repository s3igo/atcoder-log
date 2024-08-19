use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize);

    println!("{}", (0..=n).rev().join("\n"));
}
