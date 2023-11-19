use proconio::input;

fn main() {
    input!(n: usize, x: usize, a: [usize; n]);

    let ans = a.binary_search(&x).unwrap() + 1;

    println!("{ans}");
}
