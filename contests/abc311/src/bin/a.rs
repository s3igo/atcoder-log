use proconio::{input, marker::Chars};

fn main() {
    input! {
        _: usize,
        s: Chars,
    }

    let mut is_complete = (false, false, false);
    for (i, c) in s.iter().enumerate() {
        match c {
            'A' => is_complete.0 = true,
            'B' => is_complete.1 = true,
            'C' => is_complete.2 = true,
            _ => unreachable!(),
        }
        if is_complete == (true, true, true) {
            println!("{}", i + 1);
            return;
        }
    }
    println!("No");
}
