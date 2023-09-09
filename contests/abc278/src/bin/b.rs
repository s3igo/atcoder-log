use proconio::input;

fn main() {
    input! {
        mut h: usize,
        mut m: usize,
    }

    while is_valid_range(h, m) {
        if is_mistakable(h, m) {
            break;
        }

        let (next_h, next_m) = increment_time((h, m));
        h = next_h;
        m = next_m;
    }

    println!("{} {}", h, m);
}

fn is_valid_range(h: usize, m: usize) -> bool {
    (0..24).contains(&h) && (0..60).contains(&m)
}

fn is_mistakable(h: usize, m: usize) -> bool {
    let h = match h >= 10 {
        true => h.to_string(),
        false => format!("0{}", h),
    };
    let m = match m >= 10 {
        true => m.to_string(),
        false => format!("0{}", m),
    };

    let swapped_h = format!("{}{}", h.chars().next().unwrap(), m.chars().next().unwrap());
    let swapped_m = format!("{}{}", h.chars().last().unwrap(), m.chars().last().unwrap());

    is_valid_range(swapped_h.parse().unwrap(), swapped_m.parse().unwrap())
}

fn increment_time((mut h, mut m): (usize, usize)) -> (usize, usize) {
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
