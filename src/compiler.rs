use std::ffi::{c_char, CString};
use std::mem::forget;
use std::os::raw::c_void;
use std::panic::catch_unwind;
use std::path::Path;
use std::str::FromStr;
use netcorehost::hostfxr::{FunctionPtr, Hostfxr, ManagedFunction};
use netcorehost::{nethost, pdcstr};
use netcorehost::pdcstring::PdCString;

pub type Logger = extern "system" fn(Severity, i32, *const i16, i32);

pub type InjectReferenceDef = extern "system" fn(*const i16, i32, *const u8, i32) -> bool;

pub type CompilerCallbackDef = extern "system" fn(*mut c_void);

pub type InitDef = ManagedFunction<<extern "system" fn(*const u8, i32) -> bool as FunctionPtr>::Managed>;

pub type CompilerFuncDef = ManagedFunction<<extern "system" fn(*mut c_void) as FunctionPtr>::Managed>;

pub static mut LOG_METHOD: Option<Logger> = None;

pub static mut COMPILE_METHOD: Option<CompilerFuncDef> = None;

pub static mut COMPILE_METHOD_MONO_CALLBACK: Option<CompilerCallbackDef> = None;

pub static mut INJECT_REFERENCE_METHOD: Option<InjectReferenceDef> = None;

#[no_mangle]
pub unsafe extern "system" fn compile_script(args: *mut c_void)
{
	if COMPILE_METHOD.is_none() {
		println!("No compile method");
		return;
	}
	COMPILE_METHOD.as_ref().unwrap()(args);
}

#[no_mangle]
pub unsafe extern "system" fn inject_reference(str_ptr: *const i16, str_len: i32, data_ptr: *const u8, data_len: i32) -> bool
{
	if INJECT_REFERENCE_METHOD.is_none() {
		println!("No inject reference method");
		return false;
	}
	return INJECT_REFERENCE_METHOD.as_ref().unwrap()(str_ptr, str_len, data_ptr, data_len);
}

#[no_mangle]
pub unsafe extern "system" fn register_netcore_methods(inject_ref: InjectReferenceDef)
{
	INJECT_REFERENCE_METHOD = Some(inject_ref);
}
#[repr(i32)]
pub enum Severity
{
	Error,
	Warning,
	Notice,
	Debug
}

#[no_mangle]
pub unsafe extern "system" fn log_message(level: Severity, verb: i32, ptr: *mut i16, len: i32) -> bool
{
	if LOG_METHOD.is_none() {
		return false;
	}
	LOG_METHOD.as_ref().unwrap()(level, verb, ptr, len);
	return true;
}

#[no_mangle]
pub unsafe extern "system" fn register_mono_callbacks(log: Logger, comp: CompilerCallbackDef)
{
	LOG_METHOD = Some(log);
	COMPILE_METHOD_MONO_CALLBACK = Some(comp);
}

#[no_mangle]
pub unsafe extern "system" fn compile_callback(cb: *mut c_void)
{
	if COMPILE_METHOD_MONO_CALLBACK.is_none() {
		return;
	}
	COMPILE_METHOD_MONO_CALLBACK.as_ref().unwrap()(cb);
}

#[inline]
pub unsafe fn cptr_to_str(ptr: *mut c_char) -> CString
{

	let old = CString::from_raw(ptr);
	let new = old.clone();
	forget(old);
	new
}

#[no_mangle]
pub unsafe extern "system" fn initialize_runtime(runtime_cstr: *mut c_char, asm_ctr: *mut c_char) -> bool
{
	if COMPILE_METHOD.is_some() {
		return false;
	}

	let err = catch_unwind(|| {
		let cs = cptr_to_str(runtime_cstr);
		let runtime_str = cs.to_str().unwrap();
		let path = Path::new(runtime_str).join("runtime");
		let rt_path = path.to_str().unwrap();
		println!("dt_path: {}", rt_path);
		let pd = PdCString::from_str(rt_path).unwrap();
		let host: Hostfxr = match nethost::load_hostfxr_with_dotnet_root(pd) {
			Ok(r) => r,

			Err(e) => {
				println!("[CoreCLR Compiler Host]: Stage 0 failure: {:?}", e);
				return false;
			}
		};

		let path = Path::new(rt_path).join("runtimeconfig.json");
		let cfg_path = path.to_str().unwrap();
		let pd = PdCString::from_str(&cfg_path).unwrap();
		let ctx =
			match host.initialize_for_runtime_config(pd)
			{
				Ok(r) => r,

				Err(e) => {
					println!("[CoreCLR Compiler Host]: Stage 1 failure: {:?}", e);
					return false;
				}
			};

		let cs = cptr_to_str(asm_ctr);
		let pd = PdCString::from_str(cs.to_str().unwrap()).unwrap();
		let loader = match ctx.get_delegate_loader_for_assembly(pd)
		{
			Ok(r) => r,

			Err(e) => {
				println!("[CoreCLR Compiler Host]: Stage 2 failure: {:?}", e);
				return false;
			}
		};
		let compile: CompilerFuncDef = match loader.get_function::<fn(*mut c_void)>(
			pdcstr!("Carbon.Compiler.CompilerEntrypoint, Carbon.Compiler"),
			pdcstr!("_native_compile"),
			pdcstr!("Carbon.Compiler.CompilerEntrypoint+NativeCompileFunc, Carbon.Compiler"))
		{
			Ok(r) => r,

			Err(e) => {
				println!("[CoreCLR Compiler Host]: Stage 3 failure: {:?}", e);
				return false;
			}
		};
		let init: InitDef = match loader.get_function::<fn(*const u8, i32) -> bool>(
			pdcstr!("Carbon.Compiler.CompilerEntrypoint, Carbon.Compiler"),
			pdcstr!("_native_init"),
			pdcstr!("Carbon.Compiler.CompilerEntrypoint+NativeInit, Carbon.Compiler"))
		{
			Ok(r) => r,

			Err(e) => {
				println!("[CoreCLR Compiler Host]: Stage 4 failure: {:?}", e);
				return false;
			}
		};
		let managed_path = Path::new(runtime_str).join("managed");
		let mg_str = managed_path.to_str().unwrap();
		if !managed_path.exists()
		{
			println!("[CoreCLR Compiler Host] Stage 5 failure");
			return false;
		}
		if !init(mg_str.as_ptr(), mg_str.len() as i32)
		{
			println!("[CoreCLR Compiler Host] Stage 6 failure");
			return false;
		}
		COMPILE_METHOD = Some(compile);
		println!("[CoreCLR Compiler Host] initialization complete");
		return true;
	});

	match err {
		Ok(a) => a,
		Err(b) => false
	}
}