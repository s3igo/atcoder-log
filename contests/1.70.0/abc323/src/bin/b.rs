use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, s: [String; n]);

    let ans = s
        .iter()
        .map(|s| s.chars().filter(|&c| c == 'o').count())
        .zip(1..)
        .sorted_by_key(|&(x, i)| (x, -i))
        .rev()
        .map(|(_, i)| i)
        .collect_vec();

    println!("{}", ans.iter().join(" "));
}
