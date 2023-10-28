use proconio::input;

fn main() {
    input!(x: isize, y: isize);

    let cond = if x > y { x - y <= 3 } else { y - x <= 2 };

    println!("{}", if cond { "Yes" } else { "No" });
}
