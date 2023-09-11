use proconio::input;

fn main() {
    input!(n: usize);

    let divs = divisors(n);
    let ans = (0..=n)
        .map(|i| match divs.iter().find(|&x| i % (n / x) == 0).unwrap() {
            y @ ..=9 => std::char::from_digit(*y as u32, 10).unwrap(),
            _ => '-',
        })
        .collect::<String>();

    println!("{}", ans);
}

fn divisors(n: usize) -> Vec<usize> {
    let (mut from_lower, mut from_upper) = (vec![], vec![]);
    for i in (1..).take_while(|&i| i * i <= n) {
        if n % i == 0 {
            from_lower.push(i);
            if i != n / i {
                from_upper.push(n / i);
            }
        }
    }
    from_upper.reverse();
    [from_lower, from_upper].concat()
}
