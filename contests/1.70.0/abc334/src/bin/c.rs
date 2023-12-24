use std::collections::BTreeSet;

use proconio::input;

fn main() {
    input!(n: usize, k: usize, a: [usize; k]);

    let mut set: BTreeSet<_> = (1..=n).flat_map(|i| [(i, 0), (i, 1)]).collect();

    for a in a {
        set.remove(&(a, 0));
    }

    if set.len() % 2 == 1 {
        set.insert((0, 0));
    }

    let mut sum = 0;
    loop {
        let first = match set.pop_first() {
            Some((e, _)) => e,
            None => break,
        };
        let second = match set.pop_first() {
            Some((e, _)) => e,
            None => break,
        };
        sum += first.abs_diff(second);
    }

    println!("{}", sum);
}
