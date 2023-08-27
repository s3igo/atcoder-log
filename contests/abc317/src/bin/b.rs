use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [usize; n],
    }

    let ans = a
        .iter()
        .sorted()
        .tuple_windows()
        .find_map(|(&l, &r)| if l + 1 == r { None } else { Some(l + 1) })
        .unwrap();

    println!("{}", ans);
}
