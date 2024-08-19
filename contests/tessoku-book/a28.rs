use proconio::input;

fn main() {
    input!(n: usize, ta: [(char, usize); n]);
    let div = 10_000;

    let mut ans = 0;
    for (t, a) in ta {
        match t {
            '+' => ans += a,
            '-' => ans = ans.checked_sub(a).unwrap_or(ans + div - a),
            '*' => ans *= a,
            _ => unreachable!(),
        }

        ans %= div;
        println!("{ans}");
    }
}
