use proconio::input;

fn main() {
    input!(x: usize, y: usize);

    let ans = (0..).find(|i| x + 10 * i >= y).unwrap();

    println!("{ans}");
}
