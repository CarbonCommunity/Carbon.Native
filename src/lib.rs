use std::ffi::{c_char, CStr};

//mod compiler;
//mod coreclr;
//mod plugins;
//mod native_array_manager;
mod profiler;
mod mono;

pub const PROTOCOL: u64 = 0;

#[no_mangle]
pub unsafe extern "system" fn carbon_get_protocol() -> u64
{
	PROTOCOL
}

#[inline]
pub unsafe fn cptr_to_str<'a>(ptr: *mut c_char) -> &'a CStr
{
	CStr::from_ptr(ptr)
}

#[no_mangle]
pub unsafe extern "system" fn carbon_init_logger(log_method: Logger)
{
	LOG_METHOD = Some(log_method);
}

pub type Logger = extern "system" fn(Severity, i32, *const u8, i32, LogSource);

pub static mut LOG_METHOD: Option<Logger> = None;

#[no_mangle]
pub unsafe extern "system" fn log_message(level: Severity, verb: i32, ptr: *const u8, len: i32, source: LogSource)
{
	match LOG_METHOD.as_ref() {
		None => (),
		Some(x) => x(level, verb, ptr, len, source)
	}
}

pub unsafe fn read_cstr<'a>(ptr: *const c_char) -> Option<&'a str>
{
	return match ptr.is_null()  {
		true => None,
		false => Some(CStr::from_ptr(ptr).to_str().unwrap())
	};
}

pub unsafe fn read_cstr_default(ptr: *const c_char, default: impl FnOnce() -> String) -> String
{
	return match read_cstr(ptr)  {
		None => default(),
		Some(x) => x.to_string()
	};
}

pub fn log_mono_internal(severity: Severity, msg: &str, source: LogSource, verb: i32)
{
	unsafe {
		match LOG_METHOD.as_ref()
		{
			None => {
				println!("{}", match source {
					LogSource::Native => format!("[Carbon Native]: {msg}"),
					LogSource::Profiler => format!("[Carbon Profiler]: {msg}"),
				});
			}
			Some(x) => x(severity, verb, msg.as_ptr(), msg.len() as i32, source)
		}
	}
}

#[macro_export]
macro_rules! run_ffi_safe {
    ($name:literal, $default:expr, $ex:expr) => {
		match std::panic::catch_unwind( std::panic::AssertUnwindSafe(|| { $ex })) {
			Ok(a) => a,
			Err(ex) => {
				let err = match ex.downcast::<String>() {
					Ok(v) => *v,
					Err(ex) => match ex.downcast::<&str>() {
						Ok(v) => v.to_string(),
						_ => String::from("Unknown error")
					}
				};
				crate::log_mono_native_host(crate::Severity::Error, format!("Internal carbon native error in {}: \n{}", $name, err).as_str());
				$default
			}
		}
	}
}

#[inline(always)]
pub fn log_mono_native_host(severity: Severity, msg: &str)
{
	log_mono_internal(severity, msg, LogSource::Native, 1);
}

#[repr(i32)]
pub enum Severity
{
	Error,
	Warning,
	Notice,
	Debug
}

#[repr(u32)]
#[derive(Eq, PartialEq)]
pub enum LogSource
{
	Native,
	Profiler
}

#[cfg(test)]
mod tests {
	#[test]
	fn test() {
		
	}
}
