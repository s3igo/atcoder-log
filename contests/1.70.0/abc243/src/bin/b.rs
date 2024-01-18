use std::collections::HashSet;

use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n], b: [usize; n]);

    let first = a.iter().zip(b.iter()).filter(|(a, b)| a == b).count();
    let second = a.iter().collect::<HashSet<_>>().intersection(&b.iter().collect::<HashSet<_>>()).count() - first;

    println!("{first}\n{second}");
}
