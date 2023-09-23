use proconio::{input, marker::Chars};

fn main() {
    input!(s: Chars);

    let ans = s[(s.len() + 1) / 2 - 1];

    println!("{}", ans);
}
