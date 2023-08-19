use itertools::*;
use proconio::input;

fn main() {
    input! {
        n: usize,
        m: usize,
        ab: [(usize, usize); m],
    }

    let losers = ab.iter().map(|(_, b)| b).dedup().collect_vec();
    let winners = (1..=n).filter(|x| !losers.contains(&x)).collect_vec();

    println!("{}", if winners.len() > 1 { String::from("-1") } else { winners[0].to_string() });
}
