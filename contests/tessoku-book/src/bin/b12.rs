use proconio::input;

fn main() {
    input!(n: f64);

    let ans = binary_search(0., 100., 1e-3, |x| x.powi(3) + x >= n);

    println!("{}", ans);
}

fn binary_search<F>(mut ng: f64, mut ok: f64, err: f64, f: F) -> f64
where
    F: Fn(f64) -> bool,
{
    while (ok - ng).abs() > err {
        let mid = (ok + ng) / 2.;
        if f(mid) {
            ok = mid;
        } else {
            ng = mid;
        }
    }
    ok
}
