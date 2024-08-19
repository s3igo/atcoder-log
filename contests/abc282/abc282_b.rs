use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        _: usize,
        a: [String; n],
    }

    let ans = a
        .iter()
        .tuple_combinations()
        .filter(|(l, r)| l.chars().zip(r.chars()).all(|(lc, rc)| lc == 'o' || rc == 'o'))
        .count();

    println!("{}", ans);
}
