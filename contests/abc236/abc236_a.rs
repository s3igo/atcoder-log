use proconio::{input, marker::Chars};

fn main() {
    input!(mut s: Chars, a: usize, b: usize);

    s.swap(a - 1, b - 1);

    println!("{}", s.iter().collect::<String>());
}
