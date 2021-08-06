use std::process;


#[test]
fn test_main() {
    process::Command::new("cargo")
        .args(&["build", "--no-default-features"])
        .status()
        .unwrap();

    let output = process::Command::new("target/debug/abugabuga")
        .args(&["tests/fixtures/breakfast.lox"])
        .output()
        .unwrap();
    
    // assert!(output.status.success());
    let output_text = std::str::from_utf8(&output.stdout).unwrap();
    assert_eq!("beignets with cafe au lait\n", output_text);
}