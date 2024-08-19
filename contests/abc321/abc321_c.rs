use itertools::Itertools;
use proconio::{input, marker::Usize1};

fn main() {
    input!(k: Usize1);

    let ans =
        (0..=9).powerset().skip(2).map(|x| x.iter().rev().join("").parse::<usize>().unwrap()).sorted().collect_vec()[k];

    println!("{}", ans);
}
