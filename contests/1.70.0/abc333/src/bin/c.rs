use std::collections::BTreeSet;

use proconio::input;

fn main() {
    input!(n: usize);

    let mut set = BTreeSet::from([(1, 0), (1, 1), (1, 2)]);

    for _ in 1..n {
        let (min, min_idx) = set.pop_first().unwrap();
        let (mid, mid_idx) = set.pop_first().unwrap();
        let (max, max_idx) = set.pop_first().unwrap();
        let len = min.to_string().len();
        let new: usize = "1".repeat(len + 1).parse().unwrap();
        if min == max {
            set.insert((1, mid_idx));
            set.insert((1, max_idx));
        } else if min == mid {
            set.insert((1, mid_idx));
            set.insert((max, max_idx));
        } else {
            set.insert((mid, mid_idx));
            set.insert((max, max_idx));
        }
        set.insert((new, min_idx));
    }

    let ans: usize = set.iter().map(|(n, _)| n).sum();

    println!("{ans}");
}
