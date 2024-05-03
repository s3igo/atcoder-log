use cargo_snippet::snippet;

#[snippet(name = ";math_is-prime")]
fn is_prime(n: usize) -> bool {
    if n < 2 {
        return false;
    }
    for i in (2..).take_while(|&i| i * i <= n) {
        if n % i == 0 {
            return false;
        }
    }
    true
}

#[snippet(name = ";math_primes")]
fn primes(n: usize) -> Vec<usize> {
    if n == 0 {
        return vec![];
    }

    let mut is_prime = vec![true; n + 1];
    is_prime[0] = false;
    is_prime[1] = false;
    for i in (2..).take_while(|&i| i * i <= n) {
        if !is_prime[i] {
            continue;
        }
        for j in (2 * i..=n).step_by(i) {
            is_prime[j] = false;
        }
    }

    is_prime.iter().enumerate().filter(|(_, &is)| is).map(|(x, _)| x).collect()
}

// enumerate divisors of n
#[snippet(name = ";math_divisors")]
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

#[snippet(name = ";math_to-radix")]
fn to_radix(mut n: usize, radix: usize) -> Vec<usize> {
    if n == 0 {
        return vec![0];
    }
    let mut digits = vec![];
    while n != 0 {
        digits.push(n % radix);
        n /= radix;
    }
    digits.into_iter().rev().collect()
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::*;

    #[rstest]
    #[test]
    #[case(0, false)]
    #[case(1, false)]
    #[case(2, true)]
    #[case(3, true)]
    #[case(4, false)]
    #[case(5, true)]
    #[case(6, false)]
    #[case(7, true)]
    #[case(8, false)]
    #[case(9, false)]
    #[case(10, false)]
    #[case(11, true)]
    #[case(12, false)]
    #[case(13, true)]
    #[case(14, false)]
    #[case(15, false)]
    #[case(16, false)]
    #[case(17, true)]
    #[case(18, false)]
    #[case(19, true)]
    #[case(20, false)]
    fn test_is_prime(#[case] n: usize, #[case] expected: bool) {
        assert_eq!(is_prime(n), expected);
    }

    #[rstest]
    #[test]
    #[case(0, vec![])]
    #[case(1, vec![])]
    #[case(2, vec![2])]
    #[case(3, vec![2, 3])]
    #[case(4, vec![2, 3])]
    #[case(5, vec![2, 3, 5])]
    #[case(6, vec![2, 3, 5])]
    #[case(7, vec![2, 3, 5, 7])]
    #[case(8, vec![2, 3, 5, 7])]
    #[case(9, vec![2, 3, 5, 7])]
    #[case(10, vec![2, 3, 5, 7])]
    #[case(11, vec![2, 3, 5, 7, 11])]
    #[case(12, vec![2, 3, 5, 7, 11])]
    #[case(13, vec![2, 3, 5, 7, 11, 13])]
    #[case(14, vec![2, 3, 5, 7, 11, 13])]
    #[case(15, vec![2, 3, 5, 7, 11, 13])]
    #[case(16, vec![2, 3, 5, 7, 11, 13])]
    #[case(17, vec![2, 3, 5, 7, 11, 13, 17])]
    #[case(18, vec![2, 3, 5, 7, 11, 13, 17])]
    #[case(19, vec![2, 3, 5, 7, 11, 13, 17, 19])]
    #[case(20, vec![2, 3, 5, 7, 11, 13, 17, 19])]
    fn test_primes(#[case] n: usize, #[case] expected: Vec<usize>) {
        assert_eq!(primes(n), expected);
    }

    #[rstest]
    #[test]
    #[case(1, vec![1])]
    #[case(2, vec![1, 2])]
    #[case(3, vec![1, 3])]
    #[case(4, vec![1, 2, 4])]
    #[case(5, vec![1, 5])]
    #[case(6, vec![1, 2, 3, 6])]
    #[case(7, vec![1, 7])]
    #[case(8, vec![1, 2, 4, 8])]
    #[case(9, vec![1, 3, 9])]
    #[case(10, vec![1, 2, 5, 10])]
    #[case(11, vec![1, 11])]
    #[case(12, vec![1, 2, 3, 4, 6, 12])]
    #[case(13, vec![1, 13])]
    #[case(14, vec![1, 2, 7, 14])]
    #[case(15, vec![1, 3, 5, 15])]
    #[case(16, vec![1, 2, 4, 8, 16])]
    #[case(17, vec![1, 17])]
    #[case(18, vec![1, 2, 3, 6, 9, 18])]
    #[case(19, vec![1, 19])]
    #[case(20, vec![1, 2, 4, 5, 10, 20])]
    fn test_divisors(#[case] n: usize, #[case] expected: Vec<usize>) {
        assert_eq!(divisors(n), expected);
    }

    #[rstest]
    #[test]
    #[case::radix2(0, 2, vec![0])]
    #[case::radix2(1, 2, vec![1])]
    #[case::radix2(2, 2, vec![1, 0])]
    #[case::radix2(3, 2, vec![1, 1])]
    #[case::radix2(4, 2, vec![1, 0, 0])]
    #[case::radix2(5, 2, vec![1, 0, 1])]
    #[case::radix2(6, 2, vec![1, 1, 0])]
    #[case::radix2(7, 2, vec![1, 1, 1])]
    #[case::radix2(8, 2, vec![1, 0, 0, 0])]
    #[case::radix2(9, 2, vec![1, 0, 0, 1])]
    #[case::radix2(10, 2, vec![1, 0, 1, 0])]
    #[case::radix2(11, 2, vec![1, 0, 1, 1])]
    #[case::radix2(12, 2, vec![1, 1, 0, 0])]
    #[case::radix2(13, 2, vec![1, 1, 0, 1])]
    #[case::radix2(14, 2, vec![1, 1, 1, 0])]
    #[case::radix2(15, 2, vec![1, 1, 1, 1])]
    #[case::radix2(16, 2, vec![1, 0, 0, 0, 0])]
    #[case::radix3(0, 3, vec![0])]
    #[case::radix3(1, 3, vec![1])]
    #[case::radix3(2, 3, vec![2])]
    #[case::radix3(3, 3, vec![1, 0])]
    #[case::radix3(4, 3, vec![1, 1])]
    #[case::radix3(5, 3, vec![1, 2])]
    #[case::radix3(6, 3, vec![2, 0])]
    #[case::radix3(7, 3, vec![2, 1])]
    #[case::radix3(8, 3, vec![2, 2])]
    #[case::radix3(9, 3, vec![1, 0, 0])]
    #[case::radix10(9, 10, vec![9])]
    #[case::radix10(10, 10, vec![1, 0])]
    #[case::radix10(99, 10, vec![9, 9])]
    #[case::radix10(100, 10, vec![1, 0, 0])]
    #[case::radix16(9, 16, vec![9])]
    #[case::radix16(10, 16, vec![10])]
    #[case::radix16(11, 16, vec![11])]
    #[case::radix16(12, 16, vec![12])]
    #[case::radix16(13, 16, vec![13])]
    #[case::radix16(14, 16, vec![14])]
    #[case::radix16(15, 16, vec![15])]
    #[case::radix16(16, 16, vec![1, 0])]
    #[case::radix16(17, 16, vec![1, 1])]
    #[case::radix16(255, 16, vec![15, 15])]
    #[case::radix16(256, 16, vec![1, 0, 0])]
    fn test_to_radix(#[case] n: usize, #[case] radix: usize, #[case] expected: Vec<usize>) {
        assert_eq!(to_radix(n, radix), expected);
    }
}
