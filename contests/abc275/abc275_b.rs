use proconio::input;

fn main() {
    input! {
        a: u128,
        b: u128,
        c: u128,
        d: u128,
        e: u128,
        f: u128,
    }
    let div = 998244353;

    let abc = a % div * (b % div) % div;
    let abc = c % div * abc % div;

    let def = d % div * (e % div) % div;
    let def = f % div * def % div;

    let ans = abc.abs_diff(def) % div;

    // NOTE: WA
    // input! {
    //     abc: [u128; 3],
    //     def: [u128; 3],
    // }
    // let abc = abc.iter().fold(1, |acc, &x| x % div * acc) % div;
    // let def = def.iter().fold(1, |acc, &x| x % div * acc) % div;

    println!("{}", ans);
}
