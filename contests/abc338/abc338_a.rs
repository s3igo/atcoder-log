use proconio::{input, marker::Chars};

fn main() {
    input!(s: Chars);

    let (head, tail) = s.split_first().unwrap();
    let cond = head.is_uppercase() && tail.iter().all(|c| c.is_lowercase());

    println!("{}", if cond { "Yes" } else { "No" });
}
