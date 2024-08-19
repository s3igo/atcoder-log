use proconio::input;

fn main() {
    input!(a: usize, b: usize);

    let ans = match (a, b) {
        (_, 0) => "Gold",
        (0, _) => "Silver",
        _ => "Alloy",
    };

    println!("{ans}");
}
