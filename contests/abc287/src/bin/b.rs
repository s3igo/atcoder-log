use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        m: usize,
        s: [String; n],
        t: [String; m],
    }

    let ans = s
        .into_iter()
        .cartesian_product(t.into_iter().unique())
        .filter(|(s, t)| s.ends_with(t))
        .count();

    println!("{}", ans);
}
