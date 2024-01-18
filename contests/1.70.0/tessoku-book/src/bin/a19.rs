use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, w: usize, wv: [(usize, usize); n]);

    println!("{}", dp(n, w, &wv));
}

macro_rules! lazy {
    ($tuple:expr) => {
        (|| $tuple.0, || $tuple.1)
    };
}

#[memoise(i, j)]
fn dp(i: usize, j: usize, wv: &[(usize, usize)]) -> usize {
    let dp = |x, y| dp(x, y, wv);
    let (w, v) = lazy!(wv[i - 1]);
    match (i, j) {
        (0, _) => 0,
        (_, j) if j < w() => dp(i - 1, j),
        (_, j) => dp(i - 1, j).max(dp(i - 1, j - w()) + v()),
    }
}
