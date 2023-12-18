use proconio::input;

fn main() {
    input!(n: usize);

    let ans = match n {
        1..=125 => 4,
        126..=211 => 6,
        _ => 8,
    };

    println!("{ans}");
}
