use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, d: [usize; n]);

    println!("{}", d.iter().sorted().dedup().count());
}
