use std::process;

const TESTS: &'static [(&'static str, &'static str)] = &[
    ("breakfast.lox", "beignets with cafe au lait\n"),
    // ("shadowing.lox", "outer\n"),
    (
        "amigos.lox",
        "que pasa amig\nque pasa amigo\nque pasa amigoo\n",
    ),
    ("functions1.lox", ""),
    ("functions2.lox", ""),
    ("functions3.lox", "<fn areWeHavingItYet>\n"),
    ("functions4.lox", "22\n"),
    ("functions5.lox", "Do stuff\nnil\n"),
    ("upvalue1.lox", "1\n"),
    ("upvalue2.lox", "1\n"),
    (
        "upvalue3.lox",
        "return from outer\ncreate inner closure\nvalue\n",
    ),
    ("upvalue4.lox", "outside\n"),
    ("upvalue5.lox", "updated\n"),
];

#[test]
fn test_main() {
    process::Command::new("cargo")
        .args(&["build", "--no-default-features"])
        .status()
        .unwrap();

    for (file, expected_output) in TESTS {
        let output = process::Command::new("target/debug/abugabuga")
            .args(&[&format!("tests/fixtures/{}", *file)])
            .output()
            .unwrap();

        let stdout_text = std::str::from_utf8(&output.stdout).unwrap();
        let stderr_text = std::str::from_utf8(&output.stderr).unwrap();
        assert!(
            output.status.success(),
            "test '{}' didn't exit with success.\nstdout: {}\nstderr: {}",
            *file,
            stdout_text,
            stderr_text
        );
        assert_eq!(
            *expected_output, stdout_text,
            "output for test '{}' ({})\ndidn't match expected output ({})",
            *file, stdout_text, *expected_output
        );
    }
}
