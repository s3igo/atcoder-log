use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, h: [usize; n]);

    println!("{}", dp(n, &h));
}

#[memoise(n)]
fn dp(n: usize, h: &[usize]) -> usize {
    let dp = |x| dp(x, h);
    match n {
        1 => 0,
        2 => h[1].abs_diff(h[0]),
        _ => {
            let short = dp(n - 1) + h[n - 1].abs_diff(h[n - 2]);
            let long = dp(n - 2) + h[n - 1].abs_diff(h[n - 3]);
            short.min(long)
        },
    }
}
