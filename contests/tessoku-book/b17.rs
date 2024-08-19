use itertools::Itertools;
use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, h: [usize; n]);

    let dp = |x| dp(x, &h);

    let ans: Vec<_> = std::iter::successors(Some(n), |&acc| match dp(acc) {
        Some(cur) if cur - dp(acc - 1)? == h[acc - 1].abs_diff(h[acc - 2]) => Some(acc - 1),
        Some(cur) if cur - dp(acc - 2)? == h[acc - 1].abs_diff(h[acc - 3]) => Some(acc - 2),
        _ => None,
    })
    .collect();

    println!("{}\n{}", ans.len(), ans.iter().rev().join(" "));
}

#[memoise(n)]
fn dp(n: usize, h: &[usize]) -> Option<usize> {
    let dp = |x| dp(x, h);
    match n {
        ..=0 => None,
        1 => Some(0),
        2 => Some(h[1].abs_diff(h[0])),
        _ => {
            let short = dp(n - 1)? + h[n - 1].abs_diff(h[n - 2]);
            let long = dp(n - 2)? + h[n - 1].abs_diff(h[n - 3]);
            Some(short.min(long))
        },
    }
}
