use num::Integer;
use proconio::input;

fn main() {
    input!(a: usize, b: usize);

    println!("{}", if (a * b).is_odd() { "Odd" } else { "Even" });
}
