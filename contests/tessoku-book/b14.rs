use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, k: usize, a: [usize; n]);

    let (lhs, rhs) = a.split_at(n / 2);

    let rhs = rhs.iter().powerset().map(|x| x.into_iter().sum::<usize>()).sorted().collect::<Vec<_>>();

    let cond = lhs.iter().powerset().map(|x| x.into_iter().sum::<usize>()).any(|x| rhs.binary_search(&(k - x)).is_ok());

    println!("{}", if cond { "Yes" } else { "No" });
}
