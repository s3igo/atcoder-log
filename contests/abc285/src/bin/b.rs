use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        s: String,
    }

    let mut ans = vec![];
    for i in 1..n {
        let mut cnt = 0;
        for (l, r) in s.chars().zip(s.chars().skip(i)) {
            if l != r {
                cnt += 1;
            } else {
                break;
            }
        }
        ans.push(cnt);
    }

    println!("{}", ans.iter().join("\n"));
}
