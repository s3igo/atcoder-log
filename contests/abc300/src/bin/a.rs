use proconio::input;

fn main() {
    input! {
        n: usize,
        a: usize,
        b: usize,
        c: [usize; n],
    }

    let (ans, _) =
        c.iter().enumerate().map(|(i, x)| (i + 1, x)).find(|(_, x)| **x == a + b).unwrap();

    println!("{}", ans);
}
