use proconio::input;

fn main() {
    input! {
        x: usize,
        k: u32,
    }

    let ans = (0..k).fold(x, |acc, i| {
        let digits = 10_usize.pow(i);
        match acc / digits % 10 {
            0..=4 => acc / digits * digits,
            5..=9 => acc / digits * digits + digits,
            _ => unreachable!(),
        }
    });

    println!("{}", ans); // TODO: WA
}

// TODO: usizeの中での四捨五入を関数に切り出す
