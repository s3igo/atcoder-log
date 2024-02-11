use proconio::input;
use itertools::Itertools;

fn main() {
    input!(a: usize, b: usize, d: usize);

    let ans = (a..=b).step_by(d).join(" ");

    println!("{ans}");
}
