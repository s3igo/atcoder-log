use itertools::Itertools;
use proconio::input;

fn main() {
    input!(s: String);

    let cond = s.chars().positions(|c| c == 'B').map(|i| i + 1).sum::<usize>() % 2 == 1
        && s.chars().skip_while(|&c| c != 'R').skip(1).take_while(|&c| c != 'R').any(|c| c == 'K');

    println!("{}", if cond { "Yes" } else { "No" });
}
