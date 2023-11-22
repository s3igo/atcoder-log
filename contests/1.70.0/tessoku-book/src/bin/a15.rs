use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n]);

    let asc = a.iter().sorted().dedup().collect::<Vec<_>>();
    let ans = a.iter().map(|a| asc.binary_search(&a).unwrap() + 1).collect::<Vec<_>>();

    println!("{}", ans.iter().join(" "));
}
