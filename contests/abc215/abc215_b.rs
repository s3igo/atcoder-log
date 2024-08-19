use proconio::input;

fn main() {
    input!(n: usize);

    let ans = (0..).take_while(|&i| 2_usize.pow(i) <= n).last().unwrap();

    println!("{ans}");
}
