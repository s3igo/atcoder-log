use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, s: usize, a: [usize; n]);

    println!("{}", if dp(n, s, &a) { "Yes" } else { "No" });
}

#[memoise(i, j)]
fn dp(i: usize, j: usize, a: &[usize]) -> bool {
    let dp = |x, y| dp(x, y, a);
    match (i, j) {
        (0, 0) => true,
        (0, _) => false,
        _ => dp(i - 1, j) || j.checked_sub(a[i - 1]).map_or(false, |x| dp(i - 1, x)),
    }
}
