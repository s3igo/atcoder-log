use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        h: usize,
        w: usize,
        a: [[u8; w]; h],
    }

    let ans = a
        .iter()
        .map(|e| {
            e.iter().map(|&c| if c == 0 { '.' } else { (b'A' - 1 + c) as char }).collect::<String>()
        })
        .collect_vec();

    println!("{}", ans.iter().join("\n"));
}
