use proconio::input;

fn main() {
    input!(s: String);

    let cond = s.chars().skip(1).step_by(2).all(|c| c == '0');

    println!("{}", if cond { "Yes" } else { "No" });
}
