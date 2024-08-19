use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n]);

    let (_, ans) = a.iter().zip(1..).sorted().rev().nth(1).unwrap();

    println!("{ans}");
}
