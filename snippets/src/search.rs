use cargo_snippet::snippet;

mod usize {
    use super::*;

    #[snippet(name = ";search_binary_usize")]
    fn binary_search<F>(mut ng: usize, mut ok: usize, f: F) -> usize
    where
        F: Fn(usize) -> bool,
    {
        while ok.abs_diff(ng) > 1 {
            let mid = (ok + ng) / 2;
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
        assert_eq!(binary_search(0, 10, |x| x >= 5), 5);
        assert_eq!(binary_search(0, 10, |x| x >= 10), 10);
        assert_eq!(binary_search(0, 10, |x| x >= 11), 10);
        assert_eq!(binary_search(0, 10, |x| x >= 9), 9);
        assert_eq!(binary_search(0, 10, |x| x >= 1), 1);
        assert_eq!(binary_search(0, 10, |x| x >= 4), 4);
        assert_eq!(binary_search(0, 10, |x| x >= 6), 6);
        assert_eq!(binary_search(0, 10, |x| x >= 7), 7);
        assert_eq!(binary_search(0, 10, |x| x >= 8), 8);
        assert_eq!(binary_search(0, 10, |x| x >= 2), 2);
        assert_eq!(binary_search(0, 10, |x| x >= 3), 3);
    }
}

mod f64 {
    use super::*;

    #[snippet(name = ";search_binary_f64")]
    fn binary_search<F>(mut ng: f64, mut ok: f64, err: f64, f: F) -> f64
    where
        F: Fn(f64) -> bool,
    {
        while (ok - ng).abs() > err {
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
