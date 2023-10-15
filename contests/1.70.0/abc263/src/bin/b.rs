use proconio::input;

fn main() {
    input!(n: usize, p: [usize; n - 1]);

    let trace = |x| trace(&p, x, 0);

    println!("{}", trace(n));
}

fn trace(p: &[usize], idx: usize, cnt: usize) -> usize {
    match idx {
        1 => cnt,
        _ => trace(p, p[idx - 2], cnt + 1),
    }
}
