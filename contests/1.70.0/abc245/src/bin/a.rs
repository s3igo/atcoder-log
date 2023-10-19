use proconio::input;

fn main() {
    input!(ab: (usize, usize), cd: (usize, usize));

    let ans = if ab <= cd { "Takahashi" } else { "Aoki" };

    println!("{}", ans);
}
