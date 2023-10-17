use proconio::input;

fn main() {
    input!(s: String);

    let ans = std::iter::once('0').chain(s.chars()).take(4).collect::<String>();

    println!("{}", ans);
}
