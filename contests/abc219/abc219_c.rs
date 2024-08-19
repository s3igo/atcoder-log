use itertools::Itertools;
use proconio::input;
use std::collections::HashMap;

fn main() {
    input!(x: String, n: usize, s: [String; n]);

    let ord: HashMap<_, _> = x.chars().zip(0..).collect();
    let ans: Vec<_> = s.iter().sorted_by_key(|s| s.chars().map(|c| ord[&c]).collect::<Vec<_>>()).collect();

    println!("{}", ans.iter().join("\n"));
}
