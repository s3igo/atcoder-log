use std::collections::BTreeSet;

use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n]);

    let mut set = a.iter().collect::<BTreeSet<_>>();

    set.pop_last();
    let ans = set.pop_last().unwrap();

    println!("{ans}");
}
