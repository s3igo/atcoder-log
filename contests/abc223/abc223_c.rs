use proconio::input;

fn main() {
    input!(n: usize, ab: [(f64, f64); n]);

    let t = ab.iter().map(|(a, b)| a / b).sum::<f64>() / 2.;
    let (ans, _) = ab.iter().fold((0., t), |(l, t), (a, b)| (l + a.min(t * b), t - t.min(a / b)));

    println!("{ans}");
}
