use proconio::input;

fn main() {
    input!(n: usize, k: usize, a: [usize; n]);

    let ans = binary_search(0, 10_usize.pow(9), |pos| a.iter().map(|&x| pos / x).sum::<usize>() >= k);

    println!("{ans}");
}

fn binary_search<F>(mut ng: usize, mut ok: usize, f: F) -> usize
where
    F: Fn(usize) -> bool,
{
    while (ok as isize - ng as isize).abs() > 1 {
        let mid = (ok + ng) / 2;
        if f(mid) {
            ok = mid;
        } else {
            ng = mid;
        }
    }
    ok
}
