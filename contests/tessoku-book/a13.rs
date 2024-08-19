use proconio::input;

fn main() {
    input!(n: usize, k: usize, a: [usize; n]);

    let (ans, _) = (0..n).fold((0, 0), |(acc, rhs), lhs| {
        let rhs = (rhs..n).find(|&rhs| a[rhs] - a[lhs] > k).unwrap_or(n);
        (acc + rhs - lhs - 1, rhs)
    });

    println!("{ans}");
}
