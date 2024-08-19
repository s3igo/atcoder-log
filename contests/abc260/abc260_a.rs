use itertools::Itertools;
use proconio::input;

fn main() {
    input!(s: String);

    let dict = s.chars().counts();
    let ans = dict.iter().find(|(_, &v)| v == 1).map_or("-1".to_string(), |(k, _)| k.to_string());

    println!("{}", ans);
}
