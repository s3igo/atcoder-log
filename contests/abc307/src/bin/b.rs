use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        s: [String; n],
    }

    let cond = s.into_iter().permutations(2).map(|x| x.concat()).any(|x| is_palindrome(&x));

    println!("{}", if cond { "Yes" } else { "No" });
}

fn is_palindrome(s: &str) -> bool {
    let s = s.chars().collect::<Vec<char>>();
    s == s.iter().rev().copied().collect::<Vec<char>>()
}
