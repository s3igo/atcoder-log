use proconio::input;

fn main() {
    input!(n: usize, m: usize, mut a: [usize; n]);

    a.sort();
    a.push(usize::MAX);
    let a = a;

    let (ans, _) = (0..n).fold((0, 0), |(acc, rhs), lhs| {
        let rhs = (rhs..n).find(|&r| a[r] - a[lhs] >= m).unwrap_or(n);
        (acc.max(rhs - lhs), rhs)
    });

    println!("{ans}");
}
