use proconio::input;

fn main() {
    input! {
        x: usize,
        y: usize,
        n: usize,
    }

    let ans = if x * 3 <= y {
        x * n
    } else {
        n / 3 * y + n % 3 * x
    };

    println!("{}", ans);
}
