use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: String);

    let mut ans = 0;
    for s in n.chars().permutations(n.len()) {
        for (l, r) in (1..n.len()).map(|i| s.split_at(i)) {
            let check = |v: &[char]| v.iter().collect::<String>().parse::<usize>();
            if let (Ok(l), Ok(r)) = (check(l), check(r)) {
                ans = ans.max(l * r);
            }
        }
    }

    println!("{ans}");
}
