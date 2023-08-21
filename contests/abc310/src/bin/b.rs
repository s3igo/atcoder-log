use std::collections::HashSet;

use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        _: usize,
    }
    let mut data = vec![(0, HashSet::<usize>::new()); n];
    for datum in data.iter_mut() {
        input! {
            p: usize,
            c: usize,
            fs: [usize; c],
        }
        *datum = (p, fs.into_iter().collect::<HashSet<_>>());
    }

    for (i, j) in data.iter().tuple_combinations() {
        let check =
            |(p1, fs1): &(usize, HashSet<usize>), (p2, fs2): &(usize, HashSet<usize>)| -> bool {
                p1 >= p2
                    && fs2.is_superset(fs1)
                    && (p1 > p2 || !fs2.difference(fs1).collect_vec().is_empty())
            };
        if check(i, j) || check(j, i) {
            println!("Yes");
            return;
        }
    }
    println!("No");
}
