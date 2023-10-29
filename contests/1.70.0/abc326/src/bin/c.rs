use proconio::input;

fn main() {
    input!(n: usize, m: usize, mut a: [usize; n]);

    a.sort();
    a.push(usize::MAX);
    let a = a;

    let (mut ans, mut rhs) = (0, 0);
    for lhs in 0..n {
        while a[rhs] - a[lhs] < m {
            rhs += 1;
        }
        ans = ans.max(rhs - lhs);
    }

    println!("{ans}");
}
