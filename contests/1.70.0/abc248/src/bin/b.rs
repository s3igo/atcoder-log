use proconio::input;

fn main() {
    input!(a: usize, b: usize, k: usize);

    let solve = |n| solve(n, b, k, 0);

    println!("{}", solve(a));
}

fn solve(a: usize, b: usize, k: usize, cnt: usize) -> usize {
    if a < b {
        solve(a * k, b, k, cnt + 1)
    } else {
        cnt
    }
}
