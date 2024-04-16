fn main() {
	#[cfg(target_os = "linux")]
	{
		println!("cargo:rustc-link-search=native=./lib");
		println!("cargo:rustc-link-lib=dylib=monobdwgc-2.0");
	}
}