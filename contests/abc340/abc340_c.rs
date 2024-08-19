use proconio::input;

fn main() {
    input!(n: usize);

    let mut dp = vec![0; n + 1];
    dp[2] = 2;
    for i in 3..=n {
        dp[i] = i + dp[i / 2] + dp[(i + 1) / 2];
    }

    println!("{}", dp[n]);
}
