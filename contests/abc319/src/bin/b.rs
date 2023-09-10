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
    let mut res = vec![];
    for i in 1..=n {
        if i * i > n {
            break;
        }
        if n % i == 0 {
            res.push(i);
            if i != n / i {
                res.push(n / i);
            }
        }
    }
    res.sort();
    res
}
