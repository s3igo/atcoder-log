use cargo_snippet::snippet;

#[snippet(name = ";search_binary_int")]
fn binary_search_int<T, F>(mut ng: T, mut ok: T, f: F) -> T
where
    T: num::Integer + num::FromPrimitive + Copy,
    F: Fn(T) -> bool,
{
    while ng.min(ok) + T::one() < ok.max(ng) {
        let mid = (ng + ok).div(T::from_u8(2).unwrap());
        if f(mid) {
            ok = mid;
        } else {
            ng = mid;
        }
    }
    ok
}

#[snippet(name = ";search_binary_f64")]
fn binary_search_float<F>(mut ng: f64, mut ok: f64, eps: f64, f: F) -> f64
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

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_binary_search_int() {
        binary_search_int(0, 10, |x| x >= 5);
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

        assert_eq!(binary_search_int(10, 0, |x| x <= 5), 5);
    }

    macro_rules! assert_delta {
        ($x:expr, $y:expr, $d:expr) => {
            if !($x - $y < $d || $y - $x < $d) {
                panic!();
            }
        };
    }

    #[test]
    fn test_binary_search_float() {
        assert_delta!(binary_search_float(0., 10., 1e-6, |x| x >= 5.), 5., 1e-6);
        assert_delta!(binary_search_float(0., 10., 1e-6, |x| x >= 10.), 10., 1e-6);
        assert_delta!(binary_search_float(0., 10., 1e-6, |x| x >= 11.), 10., 1e-6);
        assert_delta!(binary_search_float(0., 10., 1e-6, |x| x >= 9.), 9., 1e-6);
        assert_delta!(binary_search_float(0., 10., 1e-6, |x| x >= 1.), 1., 1e-6);
        assert_delta!(binary_search_float(0., 10., 1e-6, |x| x >= 4.), 4., 1e-6);
        assert_delta!(binary_search_float(0., 10., 1e-6, |x| x >= 6.), 6., 1e-6);
        assert_delta!(binary_search_float(0., 10., 1e-6, |x| x >= 7.), 7., 1e-6);
        assert_delta!(binary_search_float(0., 10., 1e-6, |x| x >= 8.), 8., 1e-6);
        assert_delta!(binary_search_float(0., 10., 1e-6, |x| x >= 2.), 2., 1e-6);
        assert_delta!(binary_search_float(0., 10., 1e-6, |x| x >= 3.), 3., 1e-6);
    }
}
