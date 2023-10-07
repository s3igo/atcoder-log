use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, x: usize, a: [usize; n - 1]);

    let result = (0..=100)
        .filter(|&i| {
            let points = a.clone().into_iter().chain(std::iter::once(i)).collect_vec();
            points.iter().sorted().dropping(1).dropping_back(1).sum::<usize>() >= x
        })
        .min();

    let ans = match result {
        Some(x) => x.to_string(),
        None => "-1".to_string(),
    };

    println!("{}", ans);
}
