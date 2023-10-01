use itertools::Itertools;
use proconio::{input, marker::Chars};

fn main() {
    input!(s: Chars);

    for i in (0..=s.len()).rev() {
        for s in s.windows(i) {
            if s == s.iter().rev().copied().collect_vec() {
                println!("{}", s.len());
                return;
            }
        }
    }
}
