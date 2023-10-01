use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [usize; n],
    }

    let ans = a
        .iter()
        .tuple_windows()
        .flat_map(|(&prev, &next)| {
            if (prev as isize - next as isize).abs() == 1 {
                vec![prev, next]
            } else if prev < next {
                (prev..=next).collect_vec()
            } else {
                (next..=prev).rev().collect_vec()
            }
        })
        .dedup()
        .collect_vec();

    println!("{}", ans.iter().join(" "));
}
