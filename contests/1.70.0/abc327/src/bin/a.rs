use proconio::input;

fn main() {
    input!(_: usize, s: String);

    let cond = s.contains("ab") || s.contains("ba");

    println!("{}", if cond { "Yes" } else { "No" });
}
