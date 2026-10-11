/*
 *
 * Copyright (c) mono, under the MIT license rights
 *
 */

use std::borrow::Cow;
use std::ffi::c_char;
use std::mem::transmute;

use bitflags::bitflags;

use crate::profiler::MonoProfiler;
use crate::read_cstr;

#[cfg_attr(target_os = "windows", link(name = "mono-2.0-bdwgc", kind = "raw-dylib"))]
#[cfg_attr(target_os = "linux", link(name = "monobdwgc-2.0", kind = "dylib"))]
extern "C" {
	#[link_name = "mono_free"]
	pub fn __internal__mono_free(ptr: *mut ());
	pub fn mono_type_get_name_full(mtype: *const MonoType, format: MonoTypeNameFormat) -> *mut c_char;
	pub fn mono_get_corlib() -> *const MonoImage;
	pub fn mono_domain_get() -> *const MonoDomain;
	pub fn mono_gc_is_incremental() -> bool;
	pub fn mono_method_get_class(method: *const MonoMethod) -> *const MonoClass;
	pub fn mono_method_get_name(method: *const MonoMethod) -> *const c_char;
	pub fn mono_class_get_image(class: *const MonoClass) -> *const MonoImage;
	pub fn mono_class_get_type(class: *const MonoClass) -> *const MonoType;
	pub fn mono_class_get_type_token(class: *const MonoClass) -> u32;
	pub fn mono_class_instance_size(class: *const MonoClass) -> i32;
	pub fn mono_image_get_name(image: *const MonoImage) -> *const c_char;
	#[link_name = "mono_object_get_size_internal"]
	pub fn mono_object_get_size(object: *const MonoObject) -> u32;
	#[link_name = "mono_stack_walk"]
	pub fn __internal_mono_stack_walk(func: *const (), ud: *mut ());
	#[link_name = "mono_stack_walk_no_il"]
	pub fn __internal_mono_stack_walk_no_il(func: *const (), ud: *mut ());
	pub fn mono_profiler_enable_allocations() -> MonoBool;
	pub fn mono_profiler_set_exception_throw_callback(handle: MonoProfilerHandle, cb: Option<ExceptionThrownCB>);
	pub fn mono_profiler_set_gc_event_callback(handle: MonoProfilerHandle, cb: Option<GCEventCB>);
	pub fn mono_profiler_set_gc_allocation_callback(handle: MonoProfilerHandle, cb: Option<GCAllocCB>);
	pub fn mono_profiler_set_image_loaded_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&MonoProfiler, *const MonoImage)>);
	pub fn mono_profiler_set_method_enter_callback(handle: MonoProfilerHandle, cb: Option<EnterCB>);
	pub fn mono_profiler_set_method_tail_call_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&MonoProfiler, *const MonoMethod, *const MonoMethod)>);
	pub fn mono_profiler_set_method_leave_callback(handle: MonoProfilerHandle, cb: Option<ExitCB>);
	pub fn mono_profiler_set_method_exception_leave_callback(handle: MonoProfilerHandle, cb: Option<ExceptionalExitCB>);
	pub fn mono_profiler_set_method_free_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&MonoProfiler, *const MonoMethod)>);
	pub fn mono_profiler_set_call_instrumentation_filter_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&MonoProfiler, *const MonoMethod) -> CallInstrumentationFlags>);
	pub fn mono_profiler_create(info: *mut MonoProfiler) -> MonoProfilerHandle;
}

pub type EnterCB = unsafe extern "C" fn(&MonoProfiler, *const MonoMethod, *const MonoProfilerCallContext);
pub type ExitCB = unsafe extern "C" fn(&MonoProfiler, *const MonoMethod, *const MonoProfilerCallContext);
pub type ExceptionalExitCB = unsafe extern "C" fn(&MonoProfiler, *const MonoMethod, *const MonoObject);
pub type GCAllocCB = unsafe extern "C" fn(&MonoProfiler, *const MonoObject);
pub type ExceptionThrownCB = unsafe extern "C" fn(&MonoProfiler, *const MonoObject);
pub type GCEventCB = unsafe extern "C" fn(&MonoProfiler, MonoProfilerGCEvent, u32, MonoBool);

pub unsafe fn mono_free_g<T>(ptr: *mut T)
{
	__internal__mono_free(transmute(ptr));
}

pub type StackWalkFunc<T> = unsafe extern "C" fn(*const MonoMethod, native_offset: i32, il_offset: i32, is_managed: MonoBool, userdata: &mut T) -> MonoBool;

#[inline]
pub unsafe fn mono_stack_walk<const NO_IL: bool, T>(cb: StackWalkFunc<T>, data: &mut T)
{
	if NO_IL {
		__internal_mono_stack_walk_no_il(cb as *const (), data as *mut T as *mut ());
	}
	else {
		__internal_mono_stack_walk(cb as *const (), data as *mut T as *mut ());
	}
}

pub struct MonoDefaults
{
	pub corlib: *const MonoImage
}

impl MonoDefaults
{
	pub fn new() -> MonoDefaults
	{
		unsafe {
			MonoDefaults {
				corlib: mono_get_corlib()
			}
		}
	}
}

bitflags! {
	#[repr(transparent)]
	pub struct CallInstrumentationFlags : u32
	{

		///Do not instrument calls.
		const None = 0;

		/// Instrument method entries.
		const Enter = 1 << 1;

		/// Also capture a call context for method entries.
		const EnterContext = 1 << 2;

		/// Instrument method exits.
		const Leave = 1 << 3;

		/// Also capture a call context for method exits.
		const LeaveContext = 1 << 4;

		/// Instrument method exits as a result of a tail call.
		const TailCall = 1 << 5;

		///Instrument exceptional method exits.
		const ExceptionLeave = 1 << 6;
	}
}

// RuntimeMethodHandle in C#
#[repr(C)]
pub struct MonoMethod
{
	_opaque: [u8; 0]
}

const NULL_PTR: &str = "__NULLPTR";

pub unsafe fn mono_method_name(method: *const MonoMethod) -> Cow<'static, str>
{
	match read_cstr(mono_method_get_name(method)) {
		Some(name) => name.to_string_lossy(),
		None => Cow::Borrowed(NULL_PTR)
	}
}

pub unsafe fn get_method_full_name(method: *const MonoMethod, format: MonoTypeNameFormat) -> String
{
	let nameptr = mono_type_get_name_full(mono_class_get_type(mono_method_get_class(method)), format);

	let ret = match read_cstr(nameptr) {
		Some(class_name) => format!("{}::{}", class_name.to_string_lossy(), mono_method_name(method)),
		None => mono_method_name(method).into_owned()
	};
	mono_free_g(nameptr);
	ret
}

#[repr(C)]
pub struct MonoClass
{
	_opaque: [u8; 0]
}

#[repr(C)]
pub struct MonoObject
{
	_opaque: [u8; 0]
}

#[repr(C)]
struct ObjectHead
{
	vtable: *const VTableHead
}

#[repr(C)]
struct VTableHead
{
	klass: *const MonoClass
}

pub unsafe fn mono_object_class(object: *const MonoObject) -> *const MonoClass
{
	(*(*object.cast::<ObjectHead>()).vtable).klass
}

#[repr(C)]
pub struct MonoString
{
	_opaque: [u8; 0]
}

#[repr(C)]
pub struct MonoArray
{
	_opaque: [u8; 0]
}

pub type MonoProfilerGCEvent = u32;
pub const MONO_GC_EVENT_PRE_STOP_WORLD: MonoProfilerGCEvent = 6;
/**
 * When this event arrives, the GC and suspend locks are released.
 */
pub const MONO_GC_EVENT_POST_START_WORLD_UNLOCKED: MonoProfilerGCEvent = 11;

pub type MonoProfilerHandle = *const ();
pub type MonoBool = i32;

// ModuleHandle in C#
#[repr(C)]
pub struct MonoImage
{
	_opaque: [u8; 0]
}

#[repr(C)]
pub enum MonoTypeNameFormat {
	IL,
	REFLECTION,
	FullName,
	AssemblyQualified,
	ReflectionQualified
}

#[repr(C)]
pub struct MonoType
{
	_opaque: [u8; 0]
}

pub type MonoProfilerCallContext = ();
pub type MonoDomain = ();
