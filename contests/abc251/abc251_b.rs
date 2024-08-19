use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, w: usize, a: [usize; n]);

    let ans = (1..=3)
        .flat_map(|i| a.iter().combinations(i))
        .map(|x| x.into_iter().sum::<usize>())
        .filter(|&x| x <= w)
        .unique()
        .count();

    println!("{}", ans);
}
