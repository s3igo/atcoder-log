use proconio::input;

fn main() {
    input!(month: usize, day: usize, y: usize, m: usize, d: usize);

    let (y, m, d) = if d == day && m == month {
        (y + 1, 1, 1)
    } else if d == day {
        (y, m + 1, 1)
    } else {
        (y, m, d + 1)
    };

    println!("{y} {m} {d}");
}
