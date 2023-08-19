use proconio::{input, marker::Chars};

fn main() {
    input! {
        n: usize,
        d: usize,
        data: [Chars; n],
    }

    let mut ans = 0;
    let mut cnt = 0;
    for i in 0..d {
        if data.iter().all(|s| s[i] == 'o') {
            cnt += 1;
            ans = ans.max(cnt);
        } else {
            cnt = 0;
        }
    }

    println!("{}", ans);
}
