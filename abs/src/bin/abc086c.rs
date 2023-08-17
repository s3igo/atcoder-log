use proconio::input;

fn main() {
    input! {
        n: usize,
        coord: [(i32, i32, i32); n],
    }

    let mut prev = (0, 0, 0);
    for (t, x, y) in coord {
        let (pt, px, py) = prev;
        let dt = t - pt;
        let dist = (x - px).abs() + (y - py).abs();
        if dt < dist || dt % 2 != dist % 2 {
            println!("No");
            return;
        }
        prev = (t, x, y);
    }
    println!("Yes")
}
