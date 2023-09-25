use proconio::input;

fn main() {
    input! {
        r: usize,
        c: usize,
    }

    let ans = match chebyshev_distance((8, 8), (r, c)) % 2 {
        1 => "black",
        _ => "white",
    };

    println!("{}", ans);
}

fn chebyshev_distance((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> usize {
    ax.abs_diff(bx).max(ay.abs_diff(by))
}
