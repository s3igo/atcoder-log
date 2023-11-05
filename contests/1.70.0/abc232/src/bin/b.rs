use proconio::input;

fn main() {
    input!(s: String, t: String);

    let cond = (0..=26).any(|i| rot(&s, i) == t);

    println!("{}", if cond { "Yes" } else { "No" });
}

fn rot(s: &str, offset: u8) -> String {
    s.bytes()
        .map(|c| match c {
            b'a'..=b'z' => b'a' + (c - b'a' + offset) % 26,
            b'A'..=b'Z' => b'A' + (c - b'A' + offset) % 26,
            _ => c,
        } as char)
        .collect()
}
