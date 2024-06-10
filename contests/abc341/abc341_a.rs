use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize);

    let ans: String = std::iter::repeat("1").take(n + 1).intersperse("0").collect();

    println!("{ans}");
}

