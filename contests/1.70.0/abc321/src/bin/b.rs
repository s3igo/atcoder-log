use std::collections::BTreeSet;

use proconio::input;

fn main() {
    input!(n: usize, x: usize, a: [usize; n - 1]);

    // use set of tuples to multiset: (value, index)
    let a = a.iter().zip(1..).collect::<BTreeSet<_>>();
    let ans = (0..=100)
        .filter(|i| {
            let mut a = a.clone();
            a.insert((i, 0));
            a.pop_first();
            a.pop_last();
            a.into_iter().map(|(&v, _)| v).sum::<usize>() >= x
        })
        .min()
        .map_or("-1".to_string(), |x| x.to_string());

    println!("{}", ans);
}
