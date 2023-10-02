use itertools::Itertools;
use proconio::input;

fn main() {
    input!(s: String);

    let ans = match s.chars().counts().iter().find(|(_, &v)| v == 1) {
        Some((k, _)) => k.to_string(),
        None => "-1".to_string(),
    };

    println!("{}", ans);
}
