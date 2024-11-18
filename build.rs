use std::env;

fn main() {
	// fix mingw builds failing
	if env::var("CARGO_CFG_UNIX").is_ok()
	{
		println!("cargo:rustc-link-search=native=./lib");
		println!("cargo:rustc-link-lib=dylib=monobdwgc-2.0");
	}
}