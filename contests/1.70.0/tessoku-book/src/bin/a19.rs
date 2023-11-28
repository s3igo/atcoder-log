use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, w: usize, wv: [(usize, usize); n]);

    println!("{}", dp(n, w, &wv));
}

#[memoise(i, j)]
fn dp(i: usize, j: usize, wv: &[(usize, usize)]) -> usize {
    let dp = |x, y| dp(x, y, wv);
    match (i, j) {
        (0, _) => 0,
        _ => match wv[i - 1] {
            (w, v) if j >= w => dp(i - 1, j).max(dp(i - 1, j - w) + v),
            _ => dp(i - 1, j),
        },
    }
}
