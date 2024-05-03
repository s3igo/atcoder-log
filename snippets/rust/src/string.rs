use cargo_snippet::snippet;

#[snippet(name = "string_rot")]
fn rot(s: &str, offset: u8) -> String {
    s.bytes()
        .map(|c| match c {
            b'a'..=b'z' => b'a' + (c - b'a' + offset) % 26,
            b'A'..=b'Z' => b'A' + (c - b'A' + offset) % 26,
            _ => c,
        } as char)
        .collect()
}

#[test]
fn test_rot() {
    assert_eq!(rot("a", 0), "a");
    assert_eq!(rot("a", 1), "b");
    assert_eq!(rot("a", 2), "c");
    assert_eq!(rot("a", 25), "z");
    assert_eq!(rot("a", 26), "a");
    assert_eq!(rot("a", 27), "b");

    assert_eq!(rot("A", 0), "A");
    assert_eq!(rot("A", 1), "B");
    assert_eq!(rot("A", 2), "C");
    assert_eq!(rot("A", 25), "Z");
    assert_eq!(rot("A", 26), "A");
    assert_eq!(rot("A", 27), "B");

    let hoge = rot("hoge", 13);
    assert_eq!(rot(&hoge, 13), "hoge");
}
