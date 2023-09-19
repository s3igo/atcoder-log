use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: [usize; 5]);

    let ans = n.iter().unique().count();

    println!("{}", ans);
}
