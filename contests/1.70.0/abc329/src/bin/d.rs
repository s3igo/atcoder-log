use std::cmp::Ordering;

use proconio::{input, marker::Usize1};

fn main() {
    input!(n: usize, m: usize, a: [Usize1; m]);

    let (mut cnt, mut prev) = (vec![0; n], 0);
    for a in a {
        cnt[a] += 1;

        prev = match cnt[prev].cmp(&cnt[a]) {
            Ordering::Less => a,
            Ordering::Greater => prev,
            Ordering::Equal => prev.min(a),
        };

        println!("{}", prev + 1);
    }
}
