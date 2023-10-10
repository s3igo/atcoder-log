use proconio::input;

fn main() {
    input!(n: usize, p: [usize; n - 1]);

    println!("{}", trace(&p, n, 0));
}

fn trace(p: &[usize], idx: usize, cnt: usize) -> usize {
    match idx {
        1 => cnt,
        _ => trace(p, p[idx - 2], cnt + 1),
    }
}
