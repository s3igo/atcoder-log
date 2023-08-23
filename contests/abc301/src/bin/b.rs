use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [usize; n],
    }

    let mut ans = a
        .iter()
        .tuple_windows()
        .flat_map(|(&prev, &next)| {
            if (prev as isize - next as isize).abs() == 1 {
                vec![prev]
            } else if prev < next {
                (prev..next).collect_vec()
            } else {
                ((next + 1)..=prev).rev().collect_vec()
            }
        })
        .collect_vec();
    ans.push(*a.last().unwrap());

    println!("{}", ans.iter().join(" "));
}
