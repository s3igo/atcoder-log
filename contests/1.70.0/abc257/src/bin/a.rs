use proconio::input;

fn main() {
    input!(n: usize, x: usize);

    let ans = ('A'..).nth((x - 1) / n).unwrap();

    println!("{}", ans);
}
