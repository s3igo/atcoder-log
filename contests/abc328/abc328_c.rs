use itertools::Itertools;
use proconio::{input, marker::Usize1};

fn main() {
    input!(_: usize, q: usize, s: String, lr: [(Usize1, Usize1); q]);

    let cumsum = s.chars().tuple_windows().scan(0, |acc, (l, r)| {
        if l == r {
            *acc += 1;
        };
        Some(*acc)
    });
    let cumsum = std::iter::once(0).chain(cumsum).collect::<Vec<_>>();

    for (l, r) in lr {
        println!("{}", cumsum[r] - cumsum[l]);
    }
}
