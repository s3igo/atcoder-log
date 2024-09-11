use proconio::input;

fn main() {
    input!(n: usize, r#as: [(usize, char); n]);

    let mut pos = [None; 2];
    let mut sum = 0;
    for (a, s) in r#as {
        let hand = if s == 'L' { 0 } else { 1 };
        pos[hand].map(|p| sum += a.abs_diff(p));
        pos[hand] = Some(a);
    }

    println!("{sum}");
}
