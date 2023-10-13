use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, a: usize, b: usize);

    let pair = [vec!['.'; b], vec!['#'; b]];
    let row = pair.iter().cycle();

    for i in 0..n {
        let row = row.clone().skip(i % 2).take(n).flatten().join("");
        for _ in 0..a {
            println!("{}", row);
        }
    }
}
