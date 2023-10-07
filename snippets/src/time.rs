use cargo_snippet::snippet;

// check if the given time; (hh, mm) is valid
#[snippet(name = ";time_is-valid-range")]
fn is_valid_range(h: usize, m: usize) -> bool {
    (0..24).contains(&h) && (0..60).contains(&m)
}

#[test]
fn test_is_valid_range() {
    assert!(is_valid_range(0, 0));
    assert!(is_valid_range(0, 59));
    assert!(is_valid_range(23, 0));
    assert!(is_valid_range(23, 59));
    assert!(!is_valid_range(24, 0));
    assert!(!is_valid_range(0, 60));
}

// increment the given time; (hh, mm) by 1 minute
#[snippet(name = ";time_increment")]
fn increment(mut h: usize, mut m: usize) -> (usize, usize) {
    m += 1;
    if m == 60 {
        m = 0;
        h += 1;
    }
    if h == 24 {
        h = 0;
    }
    (h, m)
}

#[test]
fn test_increment_time() {
    assert_eq!(increment(0, 0), (0, 1));
    assert_eq!(increment(0, 59), (1, 0));
    assert_eq!(increment(23, 59), (0, 0));
}
