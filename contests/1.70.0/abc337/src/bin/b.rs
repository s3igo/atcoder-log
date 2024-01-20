use proconio::input;
use regex::Regex;

fn main() {
    input!(s: String);

    let cond = Regex::new(r"^A*B*C*$").unwrap().is_match(&s);

    println!("{}", if cond { "Yes" } else { "No" });
}
