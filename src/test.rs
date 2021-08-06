use std::process;

const TESTS: &'static [(&'static str, &'static str)] = &[
    ("breakfast.lox", "beignets with cafe au lait\n"),
    ("shadowing.lox", "outer\n"),
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

        assert!(
            output.status.success(),
            "test '{}' didn't exit with success.",
            *file
        );
        let output_text = std::str::from_utf8(&output.stdout).unwrap();
        assert_eq!(
            *expected_output, output_text,
            "output for test '{}' ({})\ndidn't match expected output ({})",
            *file, output_text, *expected_output
        );
    }
}
