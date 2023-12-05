use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, w: usize, wv: [(usize, usize); n]);

    println!("{}", dp(n, w, &wv));
}

#[memoise(i, j)]
fn dp(i: usize, j: usize, wv: &[(usize, usize)]) -> usize {
    let dp = |x, y| dp(x, y, wv);
    if i == 0 {
        0
    } else {
        match wv[i - 1] {
            (w, _) if j < w => dp(i - 1, j),
            (w, v) => dp(i - 1, j).max(dp(i - 1, j - w) + v),
        }
    }
}
