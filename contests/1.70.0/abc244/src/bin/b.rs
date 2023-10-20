use proconio::input;

fn main() {
    input!(_: usize, t: String);

    let (x, y, _) = t.chars().fold((0, 0, 0), |(x, y, r_cnt), c| match c {
        'S' => match r_cnt % 4 {
            0 => (x + 1, y, r_cnt),
            1 => (x, y - 1, r_cnt),
            2 => (x - 1, y, r_cnt),
            3 => (x, y + 1, r_cnt),
            _ => unreachable!(),
        },
        'R' => (x, y, r_cnt + 1),
        _ => unreachable!(),
    });

    println!("{} {}", x, y);
}
