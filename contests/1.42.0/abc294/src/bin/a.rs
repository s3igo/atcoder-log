use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [usize; n],
    }

    println!("{}", a.iter().filter(|&e| e % 2 == 0).join(" "));
}
