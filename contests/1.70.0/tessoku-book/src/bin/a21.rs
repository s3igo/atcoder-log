use memoise::memoise;
use proconio::input;

fn main() {
    input!(n: usize, pa: [(usize, usize); n]);

    let ans = (1..=n).map(|i| dp(i, i, &pa)).max().unwrap();

    println!("{ans}");
}

macro_rules! lazy {
    ($tuple:expr) => { (|| $tuple.0, || $tuple.1) };
}

#[memoise(l, r)]
fn dp(l: usize, r: usize, pa: &[(usize, usize)]) -> usize {
    let dp = |x, y| dp(x, y, pa);
    let (pl, al) = lazy!(pa[l - 2]);
    let (pr, ar) = lazy!(pa[r]);
    let pop_left = || dp(l - 1, r) + (if (l..=r).contains(&pl()) { al() } else { 0 });
    let pop_right = || dp(l, r + 1) + (if (l..=r).contains(&pr()) { ar() } else { 0 });
    match (l, r) {
        (1, n) if n == pa.len() => 0,
        (1, _) => pop_right(),
        (_, r) if r == pa.len() => pop_left(),
        (_, _) => pop_left().max(pop_right()),
    }
}
