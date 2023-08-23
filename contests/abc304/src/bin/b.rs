use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize);
    let cases = (3..=9).map(|x| 10_usize.pow(x)).collect_vec();

    let &ceil = cases.iter().find(|&&x| n < x).unwrap();
    let divisor = ceil / 1000;
    let ans = (n as f64 / divisor as f64) as usize * divisor;

    println!("{}", ans);
}
