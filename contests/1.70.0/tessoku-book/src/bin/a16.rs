use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n - 1], b: [usize; n - 2]);

    println!("{}", dp(n, &a, &b));
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
