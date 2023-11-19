use std::cmp::Ordering;

use proconio::{input, marker::Usize1};

fn main() {
    input!(n: usize, a: [usize; n], q: usize, lr: [(Usize1, Usize1); q]);

    let cumsum = a
        .iter()
        .scan((0, 0), |(win, lose), &x| {
            match x {
                0 => *lose += 1,
                1 => *win += 1,
                _ => unreachable!(),
            }
            Some((*win, *lose))
        })
        .collect::<Vec<_>>();

    for (l, r) in lr {
        let (wr, lr) = cumsum.get(r).unwrap();
        let (wl, ll) = cumsum.get(l - 1).unwrap_or(&(0, 0));
        let (win, lose): (i32, i32) = (wr - wl, lr - ll);

        let ans = match win.cmp(&lose) {
            Ordering::Greater => "win",
            Ordering::Less => "lose",
            Ordering::Equal => "draw",
        };

        println!("{ans}");
    }
}
