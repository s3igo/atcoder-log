use proconio::input;

fn main() {
    input!(n: usize, y: usize);

    let ans = (0..=n)
        .flat_map(move |i| (0..=n - i).map(move |j| (i, j, n - i - j)))
        .find(|(i, j, k)| 10000 * i + 5000 * j + 1000 * k == y)
        .map_or("-1 -1 -1".to_string(), |(a, b, c)| format!("{a} {b} {c}"));

    println!("{ans}");
}
