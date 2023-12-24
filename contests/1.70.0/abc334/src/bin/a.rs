use proconio::input;

fn main() {
    input!(b: usize, g: usize);

    let is_bat = b > g;

    println!("{}", if is_bat { "Bat" } else { "Glove" });
}
