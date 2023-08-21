use proconio::input;

fn main() {
    input! {
        n: usize,
        p: usize,
        q: usize,
        ds: [usize; n],
    }

    println!("{}", p.min(q + ds.iter().min().unwrap()));
}
