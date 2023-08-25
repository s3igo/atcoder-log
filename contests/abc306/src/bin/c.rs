use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [usize; 3 * n],
    }

    // 別解
    // let mut cnt = vec![0; n];
    // let mut ans = Vec::new();
    // for e in a {
    //     cnt[e - 1] += 1;
    //     if cnt[e - 1] == 2 {
    //         ans.push(e);
    //     }
    // }

    let ans = a
        .iter()
        .enumerate()
        .map(|(i, &e)| (i + 1, e))
        .sorted_by(|(_, a), (_, b)| a.cmp(b))
        .tuple_windows::<(_, _, _)>()
        .filter_map(|((_, l), (idx, c), (_, r))| {
            if l == c && c == r {
                Some((idx, c))
            } else {
                None
            }
        })
        .sorted_by(|(i, _), (j, _)| i.cmp(j))
        .map(|(_, e)| e)
        .collect_vec();

    println!("{}", ans.iter().join(" "));
}
