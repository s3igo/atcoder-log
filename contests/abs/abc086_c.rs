use proconio::input;

fn main() {
    input!(n: usize, txy: [(usize, usize, usize); n]);

    let cond = txy
        .into_iter()
        .try_fold((0, 0, 0), |(pt, px, py), (t, x, y)| {
            let dt = t - pt;
            let dst = manhattan_dst((px, py), (x, y));
            if dt < dst || dt % 2 != dst % 2 {
                None
            } else {
                Some((t, x, y))
            }
        })
        .is_some();

    println!("{}", if cond { "Yes" } else { "No" })
}

fn manhattan_dst((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> usize {
    ax.abs_diff(bx) + ay.abs_diff(by)
}
