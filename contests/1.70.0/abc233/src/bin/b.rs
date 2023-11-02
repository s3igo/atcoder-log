use proconio::{
    input,
    marker::{Chars, Usize1},
};

fn main() {
    input!(l: Usize1, r: Usize1, mut s: Chars);

    s[l..=r].reverse();

    println!("{}", s.iter().collect::<String>());
}
