use proconio::input;

fn main() {
    input! {
        a: usize,
        b: usize,
    }

    let ans = if a % 3 != 0 && b == a + 1 || b % 3 != 1 && a == b - 1 {
        "Yes"
    } else {
        "No"
    };

    println!("{}", ans);
}
