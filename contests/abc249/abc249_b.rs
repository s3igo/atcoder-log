use itertools::Itertools;
use proconio::input;

fn main() {
    input!(s: String);

    let cond = s.chars().any(|c| c.is_ascii_uppercase())
        && s.chars().any(|c| c.is_ascii_lowercase())
        && s.chars().all_unique();

    println!("{}", if cond { "Yes" } else { "No" });
}
