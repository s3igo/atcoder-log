use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize);

    let mut ans = vec![];
    for i in 0..=n {
        for j in 0..=n {
            for k in 0..=n {
                ans.push([i, j, k]);
            }
        }
    }

    ans.iter().filter(|v| v[0] + v[1] + v[2] <= n).sorted().for_each(|v| {
        println!("{} {} {}", v[0], v[1], v[2]);
    });
}
