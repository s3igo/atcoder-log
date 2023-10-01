use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        p: char,
        q: char,
    }
    let cases = (b'A'..=b'G').zip([0, 3, 1, 4, 1, 5, 9].iter()).collect_vec();

    let range = (p.min(q) as u8..=p.max(q) as u8).collect_vec();
    let ans =
        cases.iter().filter(|(k, _)| range.contains(k)).skip(1).map(|(_, &v)| v).sum::<usize>();

    println!("{}", ans);
}
