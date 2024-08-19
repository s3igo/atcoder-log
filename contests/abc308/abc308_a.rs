use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        s: [usize; 8],
    }

    let cond = s.iter().tuple_windows().all(|(a, b)| a <= b)
        && s.iter().all(|x| (100..=675).contains(x))
        && s.iter().all(|&x| x % 25 == 0);

    println!("{}", if cond { "Yes" } else { "No" });
}
