use memoise::memoise;
use proconio::{input, marker::Chars};

fn main() {
    input!(s: Chars, t: Chars);

    println!("{}", dp(s.len(), t.len(), &s, &t));
}

#[memoise(i, j)]
fn dp(i: usize, j: usize, s: &[char], t: &[char]) -> usize {
    let dp = |x, y| dp(x, y, s, t);
    match (i, j) {
        (0, _) | (_, 0) => 0,
        (i, j) if s[i - 1] == t[j - 1] => dp(i - 1, j - 1) + 1,
        (i, j) => dp(i - 1, j).max(dp(i, j - 1)),
    }
}
