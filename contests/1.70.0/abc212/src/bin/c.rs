use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, m: usize, a: [usize; n], mut b: [usize; m]);

    b.sort();
    let ans = a
        .iter()
        .sorted()
        .map(|a| match b.binary_search(a) {
            Ok(_) => 0,
            Err(i) => match i {
                0 => a.abs_diff(b[i]),
                i if i == m => a.abs_diff(b[i - 1]),
                _ => a.abs_diff(b[i - 1]).min(a.abs_diff(b[i])),
            },
        })
        .min()
        .unwrap();

    println!("{ans}");
}
