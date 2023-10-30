use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; 4 * n - 1]);

    let cnt = a.iter().counts();
    let (ans, _) = cnt.iter().find(|(_, &v)| v == 3).unwrap();

    println!("{ans}");
}
