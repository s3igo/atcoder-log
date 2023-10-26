use proconio::input;

fn main() {
    input!(x: isize);

    let ans = if x.is_negative() { (x - 9) / 10 } else { x / 10 };

    println!("{ans}");
}
