use itertools::Itertools;
use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n - 1], b: [usize; n - 2]);

    let dp = |x| dp(x, &a, &b);

    let ans: Vec<_> = std::iter::successors(Some(n), |&acc| match dp(acc) {
        Some(cur) if dp(acc - 1)? + a[acc - 2] == cur => Some(acc - 1),
        Some(cur) if dp(acc - 2)? + b[acc - 3] == cur => Some(acc - 2),
        _ => None,
    })
    .collect();

    println!("{}\n{}", ans.len(), ans.iter().rev().join(" "));
}

#[memoise(n)]
fn dp(n: usize, a: &[usize], b: &[usize]) -> Option<usize> {
    let dp = |x| dp(x, a, b);
    match n {
        ..=0 => None,
        1 => Some(0),
        2 => Some(a[0]),
        _ => Some((dp(n - 1)? + a[n - 2]).min(dp(n - 2)? + b[n - 3])),
    }
}
