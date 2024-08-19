use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, s: [String; n]);

    let cond = s.into_iter().permutations(2).map(|x| x.concat()).any(|x| x.chars().eq(x.chars().rev()));

    println!("{}", if cond { "Yes" } else { "No" });
}
