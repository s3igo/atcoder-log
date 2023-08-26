use proconio::input;

fn main() {
    input! {
        _: usize,
        s: String,
    }

    let cond = s.chars().any(|c| c == 'o') && s.chars().all(|c| c != 'x');

    println!("{}", if cond { "Yes" } else { "No" });
}
