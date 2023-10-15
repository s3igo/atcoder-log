use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n]);

    let cond = a.iter().all_equal();

    println!("{}", if cond { "Yes" } else { "No" });
}
