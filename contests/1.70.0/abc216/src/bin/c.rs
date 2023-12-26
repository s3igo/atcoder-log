use itertools::Itertools;
use num_integer::Integer;
use proconio::input;

fn main() {
    input!(mut n: usize);

    let mut s = vec![];
    while n != 0 {
        if n.is_even() {
            s.push('B');
            n /= 2;
        } else {
            s.push('A');
            n -= 1;
        }
    }

    println!("{}", s.iter().rev().join(""));
}
