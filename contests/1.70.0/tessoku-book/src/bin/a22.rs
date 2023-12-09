use proconio::{input, marker::Usize1};

fn main() {
    input!(n: usize, a: [Usize1; n - 1], b: [Usize1; n - 1]);

    let mut dp = vec![None; n];
    dp[0] = Some(0);
    for i in 0..n - 1 {
        let (a, b) = (a[i], b[i]);
        if let Some(cur) = dp[i] {
            dp[a] = Some(dp[a].map_or(cur + 100, |x| x.max(cur + 100)));
            dp[b] = Some(dp[b].map_or(cur + 150, |x| x.max(cur + 150)));
        }
    }

    println!("{}", dp[n - 1].unwrap());
}
