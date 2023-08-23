use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        sa: [(String, usize); n],
    }

    let min = sa.iter().map(|(_, a)| a).min().unwrap();
    let ans = sa.iter().cycle().skip_while(|(_, a)| a != min).take(n).map(|(s, _)| s).collect_vec();

    println!("{}", ans.iter().join("\n"));
}
