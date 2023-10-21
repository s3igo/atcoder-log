use itertools::iproduct;
use proconio::{input, marker::Chars};

fn main() {
    input!(h: usize, w: usize, s: [Chars; h]);

    let mut checked = vec![vec!['.'; w]; h];

    let mut cnt = 0;
    for (i, j) in iproduct!(0..h, 0..w) {
        if s[i][j] != checked[i][j] {
            trace(i, j, h, w, &s, &mut checked);
            cnt += 1;
        }
    }

    println!("{}", cnt);
}

fn trace(i: usize, j: usize, h: usize, w: usize, s: &Vec<Vec<char>>, checked: &mut Vec<Vec<char>>) {
    if s[i][j] != checked[i][j] {
        checked[i][j] = '#';
        trace(i.saturating_sub(1), j, h, w, s, checked);
        trace(i.saturating_sub(1), if j < w - 1 { j + 1 } else { j }, h, w, s, checked);
        trace(i, if j < w - 1 { j + 1 } else { j }, h, w, s, checked);
        trace(
            if i < h - 1 { i + 1 } else { i },
            if j < w - 1 { j + 1 } else { j },
            h,
            w,
            s,
            checked,
        );
        trace(if i < h - 1 { i + 1 } else { i }, j, h, w, s, checked);
        trace(if i < h - 1 { i + 1 } else { i }, j.saturating_sub(1), h, w, s, checked);
        trace(i, j.saturating_sub(1), h, w, s, checked);
        trace(i.saturating_sub(1), j.saturating_sub(1), h, w, s, checked);
    }
}
