use itertools::Itertools;
use proconio::{input, marker::Chars};

fn main() {
    input! {
        _: usize,
        s: Chars,
    }

    let ans = s.iter().flat_map(|x| vec![x, x]).collect_vec();

    println!("{}", ans.iter().join(""));
}
