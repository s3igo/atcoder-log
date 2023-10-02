use proconio::input;

fn main() {
    input!(k: usize);

    let (h, m) = (0..k).fold((21, 0), |(h, m), _| increment(h, m));

    println!("{:02}:{:02}", h, m);
}

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
