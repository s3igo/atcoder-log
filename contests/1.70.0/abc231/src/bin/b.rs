use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, s: [String; n]);

    let cnt = s.iter().counts();
    let (ans, _) = cnt.iter().max_by_key(|(_, &v)| v).unwrap();

    print!("{ans}");
}
