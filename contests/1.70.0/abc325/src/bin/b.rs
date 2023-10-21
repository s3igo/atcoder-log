use proconio::input;

fn main() {
    input!(n: usize, wx: [(usize, usize); n]);

    let ans = (0..24)
        .map(|i| {
            wx.iter().fold(
                0,
                |acc, (w, x)| if (9..18).contains(&((i + x) % 24)) { acc + w } else { acc },
            )
        })
        .max()
        .unwrap();

    println!("{}", ans);
}
