use proconio::input;

fn main() {
    input!(n: usize, h: [usize; n]);

    let mut t = 0;
    for h in h {
        let (d, r) = num_integer::div_rem(h, 5);
        t += d * 3;
        let mut h = r as i32;
        while h > 0 {
            t += 1;
            h -= if t % 3 == 0 { 3 } else { 1 };
        }
    }

    println!("{t}");
}
