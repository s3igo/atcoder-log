use proconio::input;

fn main() {
    input! {
        _: usize,
        m: usize,
        x: usize,
        t: usize,
        d: usize,
    }

    if m >= x {
        println!("{}", t);
        return;
    }

    let ans = t - d * (x - m);

    println!("{}", ans);
}
