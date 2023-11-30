use proconio::input;

fn main() {
    input!(x: usize);

    let ans = match x {
        0..=39 => (40 - x).to_string(),
        40..=69 => (70 - x).to_string(),
        70..=89 => (90 - x).to_string(),
        _ => "expert".to_string(),
    };

    println!("{ans}");
}
