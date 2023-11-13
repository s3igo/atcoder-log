use proconio::{input, marker::Usize1};

fn main() {
    input!(n: usize, ab: [(Usize1, Usize1); n - 1]);

    let cnt = ab.iter().fold(vec![0; n], |mut acc, &(a, b)| {
        acc[a] += 1;
        acc[b] += 1;
        acc
    });

    let cond = cnt.iter().any(|&x| x == n - 1);

    println!("{}", if cond { "Yes" } else { "No" });
}
