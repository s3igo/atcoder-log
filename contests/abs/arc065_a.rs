use proconio::input;
use regex::Regex;

fn main() {
    input!(s: String);

    let cond = Regex::new(r"^(dream|dreamer|erase|eraser)+$").unwrap().is_match(&s);

    println!("{}", if cond { "YES" } else { "NO" });
}
