use itertools::Itertools;
use proconio::{input, marker::Chars};

fn main() {
    input!(h: usize, w: usize, s: [Chars; h]);

    let Some((a, b)) = s
        .iter()
        .flatten()
        .positions(|&c| c == 'o')
        .map(|x| (x / w, x % w))
        .collect_tuple() else { unreachable!() };

    let ans = manhattan_dst(a, b);

    println!("{}", ans);
}

fn manhattan_dst((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> usize {
    ax.abs_diff(bx) + ay.abs_diff(by)
}
