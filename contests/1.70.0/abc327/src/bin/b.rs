use proconio::input;

fn main() {
    input!(b: usize);

    let ans = (1..16).find(|&i| (i as usize).pow(i as u32) == b).map_or("-1".to_string(), |n| n.to_string());

    println!("{ans}");
}
