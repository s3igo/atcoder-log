use itertools::{Itertools, Position};
use proconio::{input, marker::Chars};

fn main() {
    input! {
        n: usize,
        a: [Chars; n],
    }

    let mut ans = a.clone();
    for (i, e) in a.iter().tuple_windows::<(_, _)>().with_position().enumerate() {
        match e {
            Position::Only((prev, next)) => {
                ans[0][0] = next[0];
                ans[0][1] = prev[0];
                ans[1][0] = next[1];
                ans[1][1] = prev[1];
                break;
            },
            Position::First((prev, next)) => {
                ans[0].rotate_right(1);
                ans[0][0] = next[0];
                ans[1][n - 1] = prev[n - 1];
            },
            Position::Middle((prev, next)) => {
                ans[i][0] = next[0];
                ans[i + 1][n - 1] = prev[n - 1];
            },
            Position::Last((prev, next)) => {
                ans[n - 1].rotate_left(1);
                ans[n - 1][n - 1] = prev[n - 1];
                ans[n - 2][0] = next[0];
            },
        }
    }

    for e in ans {
        println!("{}", e.iter().join(""));
    }
}
