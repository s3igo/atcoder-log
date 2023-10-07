use itertools::Itertools;
use itertools_num::ItertoolsNum;
use proconio::input;

fn main() {
    input!(n: usize, m: usize, a: [usize; m], s: [String; n]);

    let points = s
        .iter()
        .map(|s| s.chars().positions(|c| c == 'o').map(|x| a[x]).sum::<usize>())
        .zip(1..)
        .map(|(x, i)| x + i)
        .collect_vec();

    for (i, s) in s.iter().enumerate() {
        let cumsum =
            s.chars().positions(|c| c == 'x').map(|x| a[x]).sorted().rev().cumsum::<usize>();
        let mut points = points.clone();
        let cur = points.remove(i);
        let &max = points.iter().max().unwrap();
        let ans = if cur > max { 0 } else { cumsum.take_while(|&x| x <= max - cur).count() + 1 };

        println!("{}", ans);
    }
}
