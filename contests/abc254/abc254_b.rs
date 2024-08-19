use itertools::Itertools;
use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize);

    for i in 0..n {
        let ans = (0..=i).map(|j| solve(i, j)).join(" ");
        println!("{}", ans);
    }
}

#[memoise(i < 30, j < 30)]
fn solve(i: usize, j: usize) -> usize {
    match (i, j) {
        (i, j) if j == 0 || i == j => 1,
        _ => solve(i - 1, j - 1) + solve(i - 1, j),
    }
}
