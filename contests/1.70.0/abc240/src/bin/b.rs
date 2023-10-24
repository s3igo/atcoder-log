use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n]);

    let ans = a.iter().unique().count();

    println!("{ans}");
}
