use proconio::input;

fn main() {
    input!(a: usize, b: usize, c: usize, d: usize);

    let ans = if (a, b) <= (c, d) { "Takahashi" } else { "Aoki" };

    println!("{}", ans);
}
