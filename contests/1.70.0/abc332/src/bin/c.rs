use proconio::{input, marker::Chars};

fn main() {
    input!(n: usize, m: usize, s: Chars);

    if check(&s, 0, m) {
        println!("0");
        return;
    }

    let ans = binary_search(0, n, |i| check(&s, i, m));

    println!("{ans}");
}

fn check(s: &Vec<char>, logo: usize, muji: usize) -> bool {
    let mut logo_used = 0;
    let mut muji_used = 0;
    for s in s {
        match s {
            '0' => {
                logo_used = 0;
                muji_used = 0;
            },
            '1' => {
                if muji > muji_used {
                    muji_used += 1;
                } else {
                    logo_used += 1;
                }
            },
            '2' => {
                logo_used += 1;
            },
            _ => unreachable!(),
        }
        if logo_used > logo {
            return false;
        }
    }
    true
}

fn binary_search<F>(mut ng: usize, mut ok: usize, f: F) -> usize
where
    F: Fn(usize) -> bool,
{
    while (ok as isize - ng as isize).abs() > 1 {
        let mid = (ok + ng) / 2;
        if f(mid) {
            ok = mid;
        } else {
            ng = mid;
        }
    }
    ok
}
