use proconio::input;

fn main() {
    input!(n: usize, txy: [(usize, usize, usize); n]);

    let cond = txy
        .into_iter()
        .try_fold((0, 0_usize, 0_usize), |(pt, px, py), (t, x, y)| {
            let dt = t - pt;
            let manhattan_dst = px.abs_diff(x) + py.abs_diff(y);
            if dt < manhattan_dst || dt % 2 != manhattan_dst % 2 {
                None
            } else {
                Some((t, x, y))
            }
        })
        .is_some();

    println!("{}", if cond { "Yes" } else { "No" })
}
