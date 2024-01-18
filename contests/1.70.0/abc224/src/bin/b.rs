use itertools::iproduct;
use proconio::input;

fn main() {
    input!(h: usize, w: usize, a: [[usize; w]; h]);

    let seq = |x| (0..x).flat_map(move |i| (i + 1..x).map(move |j| (i, j)));

    let cond = iproduct!(seq(h), seq(w)).all(|((i1, i2), (j1, j2))| a[i1][j1] + a[i2][j2] <= a[i2][j1] + a[i1][j2]);

    println!("{}", if cond { "Yes" } else { "No" });
}
