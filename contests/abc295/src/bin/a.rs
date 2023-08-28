use proconio::input;

fn main() {
    input! {
        n: usize,
        w: [String; n],
    }
    let cases = vec!["and", "not", "that", "the", "you"];

    let cond = w.iter().any(|s| cases.contains(&s.as_str()));

    println!("{}", if cond { "Yes" } else { "No" });
}
