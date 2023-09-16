use itertools::multizip;
use proconio::input;

fn main() {
    input! {
        m: usize,
        s1: String,
        s2: String,
        s3: String,
    }

    let mut cnt = vec![vec![false; m]; 3];
    for (i, c1, c2, c3) in
        multizip((0..m * 3, s1.chars().cycle(), s2.chars().cycle(), s3.chars().cycle()))
    {
        let (c1, c2, c3) = (
            c1.to_digit(10).unwrap() as usize,
            c2.to_digit(10).unwrap() as usize,
            c3.to_digit(10).unwrap() as usize,
        );

        let (mut is_changed_1, mut is_changed_2) = (false, false);
        if !cnt[0][c1] {
            cnt[0][c1] = true;
            is_changed_1 = true;
        }
        if !cnt[1][c2] && !(is_changed_1 && c2 == c1) {
            cnt[1][c2] = true;
            is_changed_2 = true;
        }
        if !cnt[2][c3] && !(is_changed_1 && c3 == c1) && !(is_changed_2 && c3 == c2) {
            cnt[2][c3] = true;
        }

        if cnt[0][c1] && cnt[1][c1] && cnt[2][c1]
            || cnt[0][c2] && cnt[1][c2] && cnt[2][c2]
            || cnt[0][c3] && cnt[1][c3] && cnt[2][c3]
        {
            println!("{}", i);
            return;
        }
    }

    println!("{}", -1); // TODO: WA
}
