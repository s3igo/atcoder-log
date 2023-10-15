use proconio::input;

fn main() {
    input!(n: usize);

    let cond = solve(n);

    println!("{}", if cond { "Yes" } else { "No" });
}

fn solve(n: usize) -> bool {
    match n {
        n if n % 3 == 0 => solve(n / 3),
        n if n % 2 == 0 => solve(n / 2),
        1 => true,
        _ => false,
    }
}
