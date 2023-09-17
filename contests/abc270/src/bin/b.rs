use proconio::input;

fn main() {
    input! {
        goal: isize,
        wall: isize,
        hammer: isize,
    }

    match (goal, wall, hammer) {
        (g, w, _) if g.abs() <= w.abs() || w.signum() != g.signum() => println!("{}", g.abs()),
        (_, w, h) if h.abs() <= w.abs() => match (h.signum(), w.signum()) {
            (hs, ws) if hs == ws => println!("{}", goal.abs()),
            _ => println!("{}", hammer.abs() * 2 + goal.abs()),
        },
        _ => println!("{}", -1),
    }
}
