use proconio::input;

fn main() {
    input!(a: usize, b: usize);

    let ans = 32_usize.pow((a - b) as u32);

    println!("{ans}");
}
