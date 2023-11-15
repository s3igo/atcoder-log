use proconio::input;

fn main() {
    input!(x: usize);

    let ans = if x == 0 || x % 100 != 0 { "No" } else { "Yes" };

    println!("{}", ans);
}
