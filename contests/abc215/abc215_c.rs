use itertools::Itertools;
use proconio::{input, marker::Usize1};

fn main() {
    input!(s: String, k: Usize1);

    let ans = s.chars().permutations(s.len()).sorted().dedup().nth(k).unwrap();

    println!("{}", ans.iter().join(""));
}
