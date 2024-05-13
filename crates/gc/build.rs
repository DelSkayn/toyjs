fn main() {
    println!("cargo::rustc-check-cfg=cfg(unstable_sanitize)");
}
