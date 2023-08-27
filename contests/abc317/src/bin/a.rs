use proconio::input;

fn main() {
    input! {
        n: usize,
        h: usize,
        x: usize,
        p: [usize; n],
    }

    let ans = p.iter().position(|e| e + h >= x).unwrap() + 1;

    println!("{}", ans);
}
