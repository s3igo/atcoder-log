use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, h: [usize; n]);

    let ans = h.iter().tuple_windows().find(|(l, r)| l >= r).map_or(h.iter().last().unwrap(), |(l, _)| l);

    println!("{ans}");
}
