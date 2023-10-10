use proconio::input;

fn main() {
    input!(n: usize);

    let divs = divisors(n);
    let ans = (0..=n)
        .map(|i| {
            divs.iter()
                .find(|&x| i % (n / x) == 0 && (1..=9).contains(x))
                .map_or('-'.to_string(), |x| x.to_string())
        })
        .collect::<String>();

    println!("{}", ans);
}

fn divisors(n: usize) -> Vec<usize> {
    let (mut lhs, mut rhs) = (vec![], vec![]);
    for i in (1..).take_while(|&i| i * i <= n) {
        if n % i == 0 {
            lhs.push(i);
            rhs.push(n / i);
        }
    }
    let lhs = lhs;
    if rhs.last() == lhs.last() {
        rhs.pop();
    }
    rhs.reverse();
    [lhs, rhs].concat()
}
