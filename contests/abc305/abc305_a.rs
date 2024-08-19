use proconio::input;

fn main() {
    input!(n: f64);

    let ans = (n / 5.).round() as usize * 5;

    println!("{}", ans);
}
