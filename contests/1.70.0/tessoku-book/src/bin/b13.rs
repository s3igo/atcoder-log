use itertools_num::ItertoolsNum;
use proconio::input;

fn main() {
    input!(n: usize, k: usize, a: [usize; n]);

    let cumsum = std::iter::once(0).chain(a.iter().cumsum()).collect::<Vec<_>>();

    let rhs = (0..n)
        .scan(0, |acc, lhs| {
            *acc = (*acc..n).find(|&r| cumsum[r + 1] - cumsum[lhs] > k).unwrap_or(n);
            Some(*acc)
        })
        .collect::<Vec<_>>();

    let ans = (0..n).map(|i| rhs[i] - i).sum::<usize>();

    println!("{ans}");
}
