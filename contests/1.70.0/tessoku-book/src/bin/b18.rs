use itertools::Itertools;
use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, mut s: usize, a: [usize; n]);

    let dp = |x, y| dp(x, y, &a);

    if !dp(n, s) {
        println!("-1");
        return;
    }

    let mut ans = vec![];
    for i in (1..=n).rev() {
        if !dp(i - 1, s) {
            ans.push(i);
            s -= a[i - 1];
        }
    }

    println!("{}\n{}", ans.len(), ans.iter().rev().join(" "));
}

#[memoise(i, j)]
fn dp(i: usize, j: usize, a: &[usize]) -> bool {
    let dp = |x, y| dp(x, y, a);
    match (i, j) {
        (0, 0) => true,
        (0, _) => false,
        _ => dp(i - 1, j) || j.checked_sub(a[i - 1]).map_or(false, |x| dp(i - 1, x)),
    }
}
