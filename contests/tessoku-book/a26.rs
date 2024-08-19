use proconio::input;

fn main() {
    input!(q: usize, x: [usize; q]);

    for x in x {
        println!("{}", if is_prime(x) { "Yes" } else { "No" });
    }
}

fn is_prime(n: usize) -> bool {
    for i in (2..).take_while(|&i| i * i <= n) {
        if n % i == 0 {
            return false;
        }
    }
    true
}
