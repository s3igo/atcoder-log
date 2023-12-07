use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, pa: [(usize, usize); n]);

    let ans = (1..=n).map(|i| dp(i, i, &pa)).max().unwrap();

    println!("{ans}");
}

#[memoise(l, r)]
fn dp(l: usize, r: usize, pa: &[(usize, usize)]) -> usize {
    let dp = |x, y| dp(x, y, pa);
    let pl = || pa[l - 2].0;
    let al = || pa[l - 2].1;
    let pr = || pa[r].0;
    let ar = || pa[r].1;
    let pop_left = || dp(l - 1, r) + (if (l..=r).contains(&pl()) { al() } else { 0 });
    let pop_right = || dp(l, r + 1) + (if (l..=r).contains(&pr()) { ar() } else { 0 });
    match (l, r) {
        (1, n) if n == pa.len() => 0,
        (1, _) => pop_right(),
        (_, r) if r == pa.len() => pop_left(),
        (_, _) => pop_left().max(pop_right()),
    }
}
