use proconio::input;

fn main() {
    input!(n: usize, x: isize, a: [isize; n - 1]);

    let ans = (0..=100)
        .filter(|&i| {
            let mut a = a.clone();
            a.push(i);
            a.sort();
            a[1..n - 1].iter().sum::<isize>() >= x
        })
        .min()
        .unwrap_or(-1);

    println!("{}", ans);
}
