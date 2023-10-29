use proconio::input;

fn main() {
    input!(x: isize, y: isize);

    let cond = x - 3 <= y && y <= x + 2;

    println!("{}", if cond { "Yes" } else { "No" });
}
