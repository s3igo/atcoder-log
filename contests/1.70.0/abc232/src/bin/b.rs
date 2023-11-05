use proconio::input;

fn main() {
    input!(s: String, t: String);

    let cond = (0..=26).any(|i| s.bytes().map(|c| b'a' + (c - b'a' + i) % 26).eq(t.bytes()));

    println!("{}", if cond { "Yes" } else { "No" });
}
