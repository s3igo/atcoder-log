use proconio::input;
use regex::Regex;

fn main() {
    input!(s: String);

    let pat = Regex::new(r"^A*B*C*$").unwrap();

    println!("{}", if pat.is_match(&s) { "Yes" } else { "No" });
}
