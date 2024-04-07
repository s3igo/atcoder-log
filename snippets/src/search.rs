use cargo_snippet::snippet;

mod usize {
    use super::*;

    #[snippet(name = ";search_binary_int")]
    fn binary_search_int<T, F>(mut ng: T, mut ok: T, f: F) -> T
    where
        T: num::Integer + num::FromPrimitive + Copy,
        F: Fn(T) -> bool,
    {
        let diff = if ok > ng { ok - ng } else { ng - ok };
        while diff > T::one() {
            let mid = (ng + ok).div(T::from_u8(2).unwrap());
            if f(mid) {
                ok = mid;
            } else {
                ng = mid;
            }
        }
        ok
    }

    #[test]
    fn test_binary_search() {
        assert_eq!(binary_search_int(0, 10, |x| x >= 5), 5);
        assert_eq!(binary_search_int(0, 10, |x| x >= 10), 10);
        assert_eq!(binary_search_int(0, 10, |x| x >= 11), 10);
        assert_eq!(binary_search_int(0, 10, |x| x >= 9), 9);
        assert_eq!(binary_search_int(0, 10, |x| x >= 1), 1);
        assert_eq!(binary_search_int(0, 10, |x| x >= 4), 4);
        assert_eq!(binary_search_int(0, 10, |x| x >= 6), 6);
        assert_eq!(binary_search_int(0, 10, |x| x >= 7), 7);
        assert_eq!(binary_search_int(0, 10, |x| x >= 8), 8);
        assert_eq!(binary_search_int(0, 10, |x| x >= 2), 2);
        assert_eq!(binary_search_int(0, 10, |x| x >= 3), 3);
    }
}

mod f64 {
    use super::*;

    #[snippet(name = ";search_binary_f64")]
    fn binary_search<F>(mut ng: f64, mut ok: f64, eps: f64, f: F) -> f64
    where
        F: Fn(f64) -> bool,
    {
        while (ok - ng).abs() > eps {
            let mid = (ok + ng) / 2.;
            if f(mid) {
                ok = mid;
            } else {
                ng = mid;
            }
        }
        ok
    }

    #[test]
    fn test_binary_search() {
        macro_rules! assert_delta {
            ($x:expr, $y:expr, $d:expr) => {
                if !($x - $y < $d || $y - $x < $d) {
                    panic!();
                }
            };
        }

        assert_delta!(binary_search(0., 10., 1e-6, |x| x >= 5.), 5., 1e-6);
        assert_delta!(binary_search(0., 10., 1e-6, |x| x >= 10.), 10., 1e-6);
        assert_delta!(binary_search(0., 10., 1e-6, |x| x >= 11.), 10., 1e-6);
        assert_delta!(binary_search(0., 10., 1e-6, |x| x >= 9.), 9., 1e-6);
        assert_delta!(binary_search(0., 10., 1e-6, |x| x >= 1.), 1., 1e-6);
        assert_delta!(binary_search(0., 10., 1e-6, |x| x >= 4.), 4., 1e-6);
        assert_delta!(binary_search(0., 10., 1e-6, |x| x >= 6.), 6., 1e-6);
        assert_delta!(binary_search(0., 10., 1e-6, |x| x >= 7.), 7., 1e-6);
        assert_delta!(binary_search(0., 10., 1e-6, |x| x >= 8.), 8., 1e-6);
        assert_delta!(binary_search(0., 10., 1e-6, |x| x >= 2.), 2., 1e-6);
        assert_delta!(binary_search(0., 10., 1e-6, |x| x >= 3.), 3., 1e-6);
    }
}
