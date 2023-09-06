use proconio::{input, marker::Chars};

fn main() {
    input!(s: Chars);

    if s.len() != 8 {
        println!("No");
        return;
    }

    let head = s[0].is_ascii_uppercase();
    let tail = s.last().unwrap().is_ascii_uppercase();
    let body = match s[1..s.len() - 1].iter().collect::<String>().parse::<usize>() {
        Ok(n) => (100_000..=999_999).contains(&n),
        Err(_) => false,
    };

    let cond = head && tail && body;

    println!("{}", if cond { "Yes" } else { "No" });
}
