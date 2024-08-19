use itertools::izip;
use proconio::input;

fn main() {
    input!(n: usize, q: [usize; n], a: [usize; n], b: [usize; n]);

    let ans = (0..=*q.iter().max().unwrap())
        .filter(|&i| !q.iter().zip(a.iter()).any(|(&q, &a)| a * i > q))
        .map(|i| {
            i + izip!(q.iter(), a.iter(), b.iter())
                .flat_map(|(&q, &a, &b)| if b == 0 { None } else { Some((q - a * i) / b) })
                .min()
                .unwrap()
        })
        .max()
        .unwrap();

    println!("{ans}");
}
