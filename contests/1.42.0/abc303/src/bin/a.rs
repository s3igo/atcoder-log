use proconio::input;

fn main() {
    input! {
        _: usize,
        s: String,
        t: String,
    }

    let is_similar = |a: char, b: char| -> bool {
        a == b || a.min(b) == '1' && a.max(b) == 'l' || a.min(b) == '0' && a.max(b) == 'o'
    };
    let cond = s.chars().zip(t.chars()).all(|(a, b)| is_similar(a, b));

    println!("{}", if cond { "Yes" } else { "No" });
}
