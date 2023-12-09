use proconio::input;

fn main() {
    input!(xy: String);

    let xy: Vec<_> = xy.split('.').collect();
    let y = match xy[1].parse::<u8>().unwrap() {
        0..=2 => "-",
        3..=6 => "",
        7..=9 => "+",
        _ => unreachable!(),
    };

    println!("{}{}", xy[0], y);
}
