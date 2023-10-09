use itertools::Itertools;
use proconio::input;

fn main() {
    input!(m: usize, s: [String; 3]);

    let ans = ('0'..='9')
        .flat_map(|i| {
            s.iter().permutations(3).filter_map(move |s_vec| {
                s_vec.iter().try_fold(0, |acc, s| {
                    s.chars().cycle().skip(acc).take(m).position(|c| c == i).map(|x| acc + x + 1)
                })
            })
        })
        .min()
        .map_or("-1".to_string(), |x| (x - 1).to_string());

    println!("{}", ans);
}
