use proconio::input;

fn main() {
    input!(y: usize);

    let ans = (y..).find(|y| y % 4 == 2).unwrap();

    println!("{}", ans);
}
