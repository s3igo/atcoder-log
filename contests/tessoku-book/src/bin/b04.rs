use proconio::input;

fn main() {
    input!(n: String);

    println!("{}", usize::from_str_radix(&n, 2).unwrap());
}
