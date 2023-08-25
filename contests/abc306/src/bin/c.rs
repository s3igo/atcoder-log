use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [usize; 3 * n],
    }

    let mut cnt = vec![0; n];
    let mut ans = Vec::new();
    for e in a {
        cnt[e - 1] += 1;
        if cnt[e - 1] == 2 {
            ans.push(e);
        }
    }

    println!("{}", ans.iter().join(" "));
}
