use proconio::input;

fn main() {
    input!(_: usize, t: String);

    let (x, y, _) = t.chars().fold((0, 0, 0), |(x, y, r_cnt), c| match (c, r_cnt % 4) {
        ('R', _) => (x, y, r_cnt + 1),
        ('S', 0) => (x + 1, y, r_cnt),
        ('S', 1) => (x, y - 1, r_cnt),
        ('S', 2) => (x - 1, y, r_cnt),
        ('S', 3) => (x, y + 1, r_cnt),
        _ => unreachable!(),
    });

    println!("{} {}", x, y);
}
