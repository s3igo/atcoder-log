use proconio::input;

fn main() {
    input! {
        n: usize,
        h: [usize; n],
    }

    let (_, ans) = h.iter().zip(1..).max_by_key(|(&h, _)| h).unwrap();

    println!("{}", ans);
}
