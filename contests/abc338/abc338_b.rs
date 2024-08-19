use itertools::Itertools;
use proconio::input;

fn main() {
    input!(s: String);

    let (_, ans) = s.chars().sorted().rev().dedup_with_count().max_by_key(|&(x, _)| x).unwrap();

    println!("{ans}");
}
