use std::ops::{Bound, RangeBounds, RangeInclusive};

use cargo_snippet::snippet;

#[snippet(name = ";time_digits_struct", prefix = "use std::ops::RangeInclusive;")]
#[derive(Debug, PartialEq)]
struct Digits {
    value: Vec<usize>,
    range: Vec<RangeInclusive<usize>>,
}

#[snippet(name = ";time_digits", include = ";time_digits_struct", prefix = "use std::ops::{Bound, RangeBounds};")]
impl Digits {
    fn new<T: RangeBounds<usize>>(value: &[usize], range: &[T]) -> Self {
        assert!(value.len() >= range.len());
        assert!(value.iter().zip(range).all(|(v, r)| r.contains(v)));

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

        let mut range: Vec<_> = range.iter().map(decide_range).collect();
        if value.len() > range.len() {
            range = std::iter::repeat(0..=usize::MAX).take(value.len() - range.len()).chain(range).collect();
        }

        Self { value: value.to_vec(), range }
    }

    fn inc(&self) -> Option<Self> {
        let mut value = self.value.clone();
        for i in (0..self.value.len()).rev() {
            if value[i] < *self.range[i].end() {
                value[i] += 1;
                break;
            } else {
                value[i] = *self.range[i].start();
            }
        }
        if value.iter().enumerate().all(|(i, v)| v == self.range[i].start()) {
            None
        } else {
            Some(Self { value, range: self.range.clone() })
        }
    }

    fn value(self) -> Vec<usize> {
        self.value
    }
}

#[test]
fn test_digits() {
    let (h, m) = (24, 60);
    assert_eq!(Digits::new(&[0, 0], &[0..h, 0..m]).inc().unwrap().value(), [0, 1]);
    assert_eq!(Digits::new(&[0, 1], &[0..h, 0..m]).inc().unwrap().value(), [0, 2]);
    assert_eq!(Digits::new(&[0, 59], &[0..h, 0..m]).inc().unwrap().value(), [1, 0]);
    assert_eq!(Digits::new(&[1, 59], &[0..h, 0..m]).inc().unwrap().value(), [2, 0]);
    assert_eq!(Digits::new(&[23, 59], &[0..h, 0..m]).inc(), None);

    assert_eq!(Digits::new(&[0, 0], &[0..h, 0..m]).inc().and_then(|d| d.inc()).unwrap().value(), [0, 2]);
    assert_eq!(
        Digits::new(&[0, 0], &[0..h, 0..m]).inc().and_then(|d| d.inc()).and_then(|d| d.inc()).unwrap().value(),
        [0, 3]
    );

    assert_eq!(
        Digits::new(&[0, 9, 9], &[0..10, 0..10])
            .inc()
            .and_then(|d| d.inc())
            .and_then(|d| d.inc())
            .and_then(|d| d.inc())
            .unwrap()
            .value(),
        [1, 0, 3]
    );

    let mut date = Some(Digits::new(&[1, 1], &[1..=12, 1..=30]));
    for _ in 0..50 {
        date = date.and_then(|d| d.inc());
    }
    assert_eq!(date.unwrap().value(), [2, 21]);

    // 3 digits
    let (h, m, s) = (24, 60, 60);
    assert_eq!(
        Digits::new(&[0, 0, 0], &[0..h, 0..m, 0..s]).inc().and_then(|d| d.inc()).and_then(|d| d.inc()).unwrap().value(),
        [0, 0, 3]
    );
    assert_eq!(Digits::new(&[0, 59, 59], &[0..h, 0..m, 0..s]).inc().and_then(|d| d.inc()).unwrap().value(), [1, 0, 1]);
}
