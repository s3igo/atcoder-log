use proconio::input;

fn main() {
    input!(k: usize, g: usize, m: usize);

    let mut cur_g = 0;
    let mut cur_m = 0;
    for _ in 0..k {
        if cur_g == g {
            cur_g = 0;
        } else if cur_m == 0 {
            cur_m = m;
        } else {
            let g_space = g - cur_g;
            if g_space >= cur_m {
                cur_g += cur_m;
                cur_m = 0;
            } else {
                cur_m -= g_space;
                cur_g = g;
            }
        }
    }

    println!("{} {}", cur_g, cur_m);
}
