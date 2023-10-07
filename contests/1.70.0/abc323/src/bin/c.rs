use itertools::Itertools;
use itertools_num::ItertoolsNum;
use proconio::input;

fn main() {
    input!(n: usize, m: usize, a: [isize; m], s: [String; n]);

    let points = s
        .iter()
        .map(|s| s.chars().positions(|c| c == 'o').fold(0, |acc, x| acc + a[x]))
        .zip(1..)
        .map(|(x, i)| x + i)
        .collect_vec();

    for i in 0..n {
        let cumsum = s[i]
            .chars()
            .positions(|c| c == 'x')
            .map(|x| a[x])
            .sorted()
            .rev()
            .cumsum::<isize>()
            .collect_vec();
        let mut points = points.clone();
        let cur = points.remove(i);
        let &max = points.iter().max().unwrap();
        let ans =
            if cur > max { 0 } else { cumsum.iter().take_while(|&&x| x <= max - cur).count() + 1 };

        println!("{}", ans);
    }
}
