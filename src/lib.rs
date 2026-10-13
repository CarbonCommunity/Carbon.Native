/*
 *
 * Copyright (c) 2023-2024 Patrette, under the GNU v3 license rights
 * Copyright (c) 2023-2024 Carbon Community, under the GNU v3 license rights
 *
 */

use std::ffi::{c_char, CStr};
use std::sync::RwLock;

mod profiler;
mod mono;
mod recording;

pub const PROTOCOL: u64 = 5;

#[no_mangle]
pub unsafe extern "system" fn carbon_get_protocol() -> u64
{
	PROTOCOL
}

#[no_mangle]
pub unsafe extern "system" fn carbon_init_logger(log_method: Logger)
{
	if let Ok(mut logger) = LOG_METHOD.write() {
		*logger = Some(log_method);
	}
}

pub type Logger = extern "system" fn(Severity, i32, *const u8, i32, LogSource);

static LOG_METHOD: RwLock<Option<Logger>> = RwLock::new(None);

fn logger() -> Option<Logger>
{
	LOG_METHOD.read().ok().and_then(|logger| *logger)
}

#[no_mangle]
pub unsafe extern "system" fn log_message(level: Severity, verb: i32, ptr: *const u8, len: i32, source: LogSource)
{
	match logger() {
		None => (),
		Some(x) => x(level, verb, ptr, len, source)
	}
}
#[macro_export]
macro_rules! ok_or_ret {
	($expr:expr, $default:expr) => {
		match $expr {
			Err(_) => {return $default;},
			Ok(x) => x
		}
	};
}
#[macro_export]
macro_rules! some_or_ret {
	($expr:expr, $default:expr) => {
		match $expr {
			None => {return $default;},
			Some(x) => x
		}
	};
}

pub unsafe fn read_cstr<'a>(ptr: *const c_char) -> Option<&'a CStr>
{
	match ptr.is_null()  {
		true => None,
		false => Some(CStr::from_ptr(ptr))
	}
}

pub unsafe fn read_cstr_default(ptr: *const c_char, default: impl FnOnce() -> String) -> String
{
	match read_cstr(ptr)  {
		None => default(),
		Some(x) => x.to_string_lossy().into_owned()
	}
}

pub fn log_mono_internal(severity: Severity, msg: &str, source: LogSource, verb: i32)
{
	match logger()
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
