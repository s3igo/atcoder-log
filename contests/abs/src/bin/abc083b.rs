use proconio::input;

fn main() {
    input! {
        n: usize,
        a: usize,
        b: usize,
    }

    let mut ans = 0_usize;
    for i in 1..=n {
        let sum = i.to_string().chars().map(|c| c.to_digit(10).unwrap()).fold(0, |acc, x| acc + x) as usize;
        if a <= sum && sum <= b {
            ans += i;
        }
    }

    println!("{}", ans);
}
