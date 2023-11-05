use proconio::input;

fn main() {
    input!(b: usize);

    let mut ans = None;
    for i in 1.. {
        let n = (i as usize).pow(i as u32);
        if n > b {
            ans = None;
            break;
        } else if n == b {
            ans = Some(i);
            break;
        }
    }

    let ans = ans.map_or("-1".to_string(), |n| n.to_string());

    println!("{ans}");
}
