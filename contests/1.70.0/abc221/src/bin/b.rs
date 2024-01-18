use proconio::{input, marker::Chars};

fn main() {
    input!(s: Chars, t: Chars);

    let cond = s == t
        || (0..s.len() - 1).any(|i| {
            let mut s = s.clone();
            s.swap(i, i + 1);
            s == t
        });

    println!("{}", if cond { "Yes" } else { "No" });
}
