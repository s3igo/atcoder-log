use proconio::input;

fn main() {
    input! {
        n: usize,
        q: usize,
        event: [(usize, usize); q],
    }

    let mut players = vec![0; n];
    for (kind, num) in event {
        match kind {
            1 => players[num - 1] += 1,
            2 => players[num - 1] += 2,
            3 => println!("{}", if players[num - 1] >= 2 { "Yes" } else { "No" }),
            _ => unreachable!(),
        }
    }
}
