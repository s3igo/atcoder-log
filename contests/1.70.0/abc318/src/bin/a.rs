use proconio::input;

fn main() {
    input!(n: isize, m: isize, p: isize);

    let ans = (n - m + p) / p;

    println!("{}", ans);
}
