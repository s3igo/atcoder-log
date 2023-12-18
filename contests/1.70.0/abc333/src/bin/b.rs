use proconio::{input, marker::Bytes};

fn main() {
    input!(s: Bytes, t: Bytes);

    let cond = distance(s[0], s[1]) == distance(t[0], t[1]);

    println!("{}", if cond { "Yes" } else { "No" });
}

fn distance(a: u8, b: u8) -> u8 {
    let diff = a.abs_diff(b);
    diff.min(5 - diff)
}
