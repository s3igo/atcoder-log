use itertools_num::ItertoolsNum;
use proconio::input;

fn main() {
    input! {
        n: usize,
        k: usize,
        a: [usize; n],
    }

    let cumsum = a.iter().cumsum().collect::<Vec<usize>>();

    let (mut l, mut r, mut cnt) = (0, 0, 0);
    while l < n - 1 {
        while r < n - 1 && cumsum[r] - cumsum.get((l as isize - 1) as usize).unwrap_or(&0) <= k {
            r += 1;
        }
        cnt += r - l;
        l += 1;
    }

    println!("{}", cnt); // TODO: WA
}
