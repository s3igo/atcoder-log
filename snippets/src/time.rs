use std::ops::{Bound, RangeBounds, RangeInclusive};

use cargo_snippet::snippet;

#[snippet(name = ";time_digits2_struct", prefix = "use std::ops::RangeInclusive;")]
#[derive(Debug, PartialEq)]
struct Digits2 {
    value: (usize, usize),
    range: (RangeInclusive<usize>, RangeInclusive<usize>),
}

#[snippet(
    name = ";time_digits2",
    include = ";time_digits2_struct",
    prefix = "use std::ops::{Bound, RangeBounds};"
)]
impl Digits2 {
    fn new<T: RangeBounds<usize>>(value: (usize, usize), range: (T, T)) -> Self {
        assert!(range.0.contains(&value.0));
        assert!(range.1.contains(&value.1));

        let decide_range = |r: &T| {
            let start = match r.start_bound() {
                Bound::Included(&s) => s,
                Bound::Excluded(&s) => s + 1,
                Bound::Unbounded => 0,
            };
            let end = match r.end_bound() {
                Bound::Included(&e) => e,
                Bound::Excluded(&e) => e - 1,
                Bound::Unbounded => unreachable!(),
            };
            start..=end
        };

        Self { value, range: (decide_range(&range.0), decide_range(&range.1)) }
    }

    fn wrap(&self) -> Option<Self> {
        Some(Self::new(self.value, self.range.clone()))
    }

    fn inc(&self) -> Option<Self> {
        let (mut a, mut b) = self.value;
        b += 1;
        if b == self.range.1.end() + 1 {
            b = *self.range.1.start();
            a += 1;
        }
        if a == self.range.0.end() + 1 {
            None
        } else {
            Some(Self::new((a, b), self.range.clone()))
        }
    }

    fn value(&self) -> (usize, usize) {
        self.value
    }
}

#[test]
fn test_digits2() {
    let (h, m) = (24, 60);
    assert_eq!(Digits2::new((0, 0), (0..h, 0..m)).inc().unwrap().value(), (0, 1));
    assert_eq!(Digits2::new((0, 1), (0..h, 0..m)).inc().unwrap().value(), (0, 2));
    assert_eq!(Digits2::new((0, 59), (0..h, 0..m)).inc().unwrap().value(), (1, 0));
    assert_eq!(Digits2::new((1, 59), (0..h, 0..m)).inc().unwrap().value(), (2, 0));
    assert_eq!(Digits2::new((23, 59), (0..h, 0..m)).inc(), None);

    assert_eq!(
        Digits2::new((0, 0), (0..h, 0..m)).inc().and_then(|d| d.inc()).unwrap().value(),
        (0, 2)
    );
    assert_eq!(
        Digits2::new((0, 0), (0..h, 0..m))
            .inc()
            .and_then(|d| d.inc())
            .and_then(|d| d.inc())
            .unwrap()
            .value(),
        (0, 3)
    );

    let mut date = Digits2::new((1, 1), (1..=12, 1..=30)).wrap();
    for _ in 0..50 {
        date = date.and_then(|d| d.inc());
    }
    assert_eq!(date.unwrap().value(), (2, 21));
}
