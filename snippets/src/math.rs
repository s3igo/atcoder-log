use cargo_snippet::snippet;

// calculate greatest common divisor
#[snippet(name = ";math_gcd")]
fn gcd(a: usize, b: usize) -> usize {
    match b {
        0 => a,
        _ => gcd(b, a % b),
    }
}

#[test]
fn test_gcd() {
    assert_eq!(gcd(0, 0), 0);
    assert_eq!(gcd(1, 0), 1);
    assert_eq!(gcd(0, 1), 1);
    assert_eq!(gcd(1, 1), 1);
    assert_eq!(gcd(2, 1), 1);
    assert_eq!(gcd(1, 2), 1);
    assert_eq!(gcd(2, 2), 2);
    assert_eq!(gcd(2, 3), 1);
    assert_eq!(gcd(3, 2), 1);
    assert_eq!(gcd(2, 4), 2);
    assert_eq!(gcd(4, 2), 2);
    assert_eq!(gcd(3, 4), 1);
    assert_eq!(gcd(4, 3), 1);
    assert_eq!(gcd(4, 6), 2);
    assert_eq!(gcd(6, 4), 2);
    assert_eq!(gcd(6, 9), 3);
    assert_eq!(gcd(9, 6), 3);
    assert_eq!(gcd(12, 18), 6);
    assert_eq!(gcd(18, 12), 6);
}

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

#[test]
fn test_is_prime() {
    assert!(!is_prime(0));
    assert!(!is_prime(1));
    assert!(is_prime(2));
    assert!(is_prime(3));
    assert!(!is_prime(4));
    assert!(is_prime(5));
    assert!(!is_prime(6));
    assert!(is_prime(7));
    assert!(!is_prime(8));
    assert!(!is_prime(9));
    assert!(!is_prime(10));
    assert!(is_prime(11));
    assert!(!is_prime(12));
    assert!(is_prime(13));
    assert!(!is_prime(14));
    assert!(!is_prime(15));
    assert!(!is_prime(16));
    assert!(is_prime(17));
    assert!(!is_prime(18));
    assert!(is_prime(19));
    assert!(!is_prime(20));
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

#[test]
fn test_primes() {
    assert_eq!(primes(0), vec![]);
    assert_eq!(primes(1), vec![]);
    assert_eq!(primes(2), vec![2]);
    assert_eq!(primes(3), vec![2, 3]);
    assert_eq!(primes(4), vec![2, 3]);
    assert_eq!(primes(5), vec![2, 3, 5]);
    assert_eq!(primes(6), vec![2, 3, 5]);
    assert_eq!(primes(7), vec![2, 3, 5, 7]);
    assert_eq!(primes(8), vec![2, 3, 5, 7]);
    assert_eq!(primes(9), vec![2, 3, 5, 7]);
    assert_eq!(primes(10), vec![2, 3, 5, 7]);
    assert_eq!(primes(11), vec![2, 3, 5, 7, 11]);
    assert_eq!(primes(12), vec![2, 3, 5, 7, 11]);
    assert_eq!(primes(13), vec![2, 3, 5, 7, 11, 13]);
    assert_eq!(primes(14), vec![2, 3, 5, 7, 11, 13]);
    assert_eq!(primes(15), vec![2, 3, 5, 7, 11, 13]);
    assert_eq!(primes(16), vec![2, 3, 5, 7, 11, 13]);
    assert_eq!(primes(17), vec![2, 3, 5, 7, 11, 13, 17]);
    assert_eq!(primes(18), vec![2, 3, 5, 7, 11, 13, 17]);
    assert_eq!(primes(19), vec![2, 3, 5, 7, 11, 13, 17, 19]);
    assert_eq!(primes(20), vec![2, 3, 5, 7, 11, 13, 17, 19]);
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

#[test]
fn test_divisors() {
    assert_eq!(divisors(1), vec![1]);
    assert_eq!(divisors(2), vec![1, 2]);
    assert_eq!(divisors(3), vec![1, 3]);
    assert_eq!(divisors(4), vec![1, 2, 4]);
    assert_eq!(divisors(5), vec![1, 5]);
    assert_eq!(divisors(6), vec![1, 2, 3, 6]);
    assert_eq!(divisors(7), vec![1, 7]);
    assert_eq!(divisors(8), vec![1, 2, 4, 8]);
    assert_eq!(divisors(9), vec![1, 3, 9]);
    assert_eq!(divisors(10), vec![1, 2, 5, 10]);
    assert_eq!(divisors(11), vec![1, 11]);
    assert_eq!(divisors(12), vec![1, 2, 3, 4, 6, 12]);
    assert_eq!(divisors(13), vec![1, 13]);
    assert_eq!(divisors(14), vec![1, 2, 7, 14]);
    assert_eq!(divisors(15), vec![1, 3, 5, 15]);
    assert_eq!(divisors(16), vec![1, 2, 4, 8, 16]);
    assert_eq!(divisors(17), vec![1, 17]);
    assert_eq!(divisors(18), vec![1, 2, 3, 6, 9, 18]);
    assert_eq!(divisors(19), vec![1, 19]);
    assert_eq!(divisors(20), vec![1, 2, 4, 5, 10, 20]);
}
