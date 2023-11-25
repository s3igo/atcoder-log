use itertools::Itertools;
use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n - 1], b: [usize; n - 2]);

    let dp = |x| dp(x, &a, &b);

    let ans: Vec<_> = std::iter::successors(Some(n), |&acc| match acc {
        p @ 2.. if dp(p - 1) + a[p - 2] == dp(p) => Some(p - 1),
        p @ 3.. if dp(p - 2) + b[p - 3] == dp(p) => Some(p - 2),
        _ => None,
    })
    .collect();

    println!("{}\n{}", ans.len(), ans.iter().rev().join(" "));
}

#[memoise(n)]
fn dp(n: usize, a: &[usize], b: &[usize]) -> usize {
    let dp = |x| dp(x, a, b);
    match n {
        1 => 0,
        2 => a[0],
        _ => (dp(n - 1) + a[n - 2]).min(dp(n - 2) + b[n - 3]),
    }
}
