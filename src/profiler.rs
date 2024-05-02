/*
 *
 * Copyright (c) 2024 Carbon Community
 * Copyright (c) 2024 Patrette
 * All rights reserved.
 *
 */

use std::{fs, mem, ptr, slice, thread};
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::{Entry, Iter};
use std::ffi::CStr;
use std::mem::transmute;
use std::sync::{Mutex, RwLock};
use std::time::{Duration, Instant};

use bitflags::bitflags;
use serde_derive::{Deserialize, Serialize};

use crate::{assert_main_domain, deref_or_ret, ok_or_ret, read_cstr, read_cstr_default, run_ffi_safe, some_or_ret};
use crate::mono::*;

thread_local! {
	pub static THREAD_DATA: Cell<*mut ThreadData> = const { Cell::new(ptr::null_mut()) }
}

#[repr(u8)]
pub enum ProfilerResultCode
{
	OK = 0,
	InvalidArgs = 1,
	Aborted = 2,
	MainThreadOnly = 3,
	NotInitialized = 4,
	CorruptedState = 5,
	UnknownError = 6,
}

pub struct StackFrame
{
	pub method: *const MonoMethod,
	pub own_allocations: u64,
	pub total_allocations: u64,
	pub time: Option<StackTiming>
}

pub struct StackTiming
{
	pub time: Instant,
	pub others: Duration
}

impl StackFrame
{
	#[inline]
	pub fn new(method: *const MonoMethod, timing: Option<StackTiming>) -> Self
	{
		StackFrame
		{
			own_allocations: 0,
			total_allocations: 0,
			time: timing,
			method
		}
	}
}

pub struct ThreadData
{
	pub main: bool,
	pub stack: Vec<StackFrame>,
	pub domain: *const MonoDomain,
	pub call_recording: HashMap<*const MonoMethod, MethodResult>,
	pub memory_recording: HashMap<*const MonoClass, MemoryResult>,
	pub gc_clock: Option<Instant>,
	// Only used from the main thread
	pub gc_timings: GCTimings
}

impl Default for ThreadData {
	fn default() -> Self {
		unsafe {
			Self {
				main: false,
				stack: Vec::with_capacity(128),
				domain: ptr::null(),
				call_recording: HashMap::with_capacity(128),
				memory_recording: HashMap::with_capacity(64),
				gc_clock: None,
				gc_timings: Default::default()
			}
		}
	}
}

pub fn get_thread_data<'a>() -> &'a mut ThreadData
{
	unsafe {
		let mut ptr = THREAD_DATA.get();
		if ptr.is_null()
		{
			ptr = Box::leak(Box::default());

			(*ptr).domain = mono_domain_get();
			
			let profiler = PROFILER.as_ref().unwrap();
			
			profiler.threads.lock().unwrap().push(&mut *ptr);
			
			THREAD_DATA.set(ptr);
		}
		&mut *ptr
	}
}

pub struct MonoProfiler
{
	pub profiler_recording: bool,
	pub main_domain: *const MonoDomain,
	pub args: ProfilerArgs,
	pub handle: MonoProfilerHandle,
	pub sync: RwLock<ProfilerSynced>,
	pub mono_defaults: MonoDefaults,
	pub threads: Mutex<Vec<&'static mut ThreadData>>,
	pub callbacks: Option<ManagedCallbacks>,
}

pub struct ProfilerSynced
{
	pub config: ProfilerConfig,
	pub profiler_images: HashSet<*const MonoImage>,
	pub profiler_method_image_map: HashMap<*const MonoMethod, *const MonoImage>,
	pub profiler_method_detour_map: HashMap<*const MonoMethod, *const MonoMethod>,
	pub runtime: Option<RuntimeData>,
}

pub type BasicIterCallback = extern "system" fn(*mut (), u64, &mut BasicIter, unsafe extern "system" fn(&mut BasicIter, &mut BasicRecord) -> bool);
pub type AdvancedIterCallback = extern "system" fn(*mut (), u64, &mut AdvIter, unsafe extern "system" fn(&mut AdvIter, &mut AdvancedRecord) -> bool);
pub type MemoryIterCallback = extern "system" fn(*mut (), u64, &mut MemoryIter, unsafe extern "system" fn(&mut MemoryIter, &mut MemoryRecord) -> bool);

pub type ManagedStringMarshalFunc = extern "system" fn(&mut *const MonoString, *const u8, i32);
pub type ManagedArrMarshalFunc<T> = extern "system" fn(&mut *const TypedMonoArray<T>, *const T, u64);

#[repr(C)]
#[derive(Copy, Clone)]
pub struct ManagedCallbacks
{
	pub string_marshal: ManagedStringMarshalFunc,
	pub bytes_marshal: ManagedArrMarshalFunc<u8>,
	
	pub basic_iter: BasicIterCallback,
	pub adv_iter: AdvancedIterCallback,
	pub mem_iter: MemoryIterCallback
}

#[derive(Serialize, Deserialize, Default)]
pub struct ProfilerConfig
{
	#[serde(rename = "Enabled")]
	pub enabled: bool,
	#[serde(rename = "TrackCalls")]
	pub instrument_methods: bool,
	#[serde(rename = "SourceViewer")]
	pub sourceviewer: bool,
	#[serde(rename = "Assemblies")]
	pub assemblies: Vec<String>,
	#[serde(rename = "Plugins")]
	pub plugins: Vec<String>,
	#[serde(rename = "Modules")]
	pub modules: Vec<String>,
	#[serde(rename = "Extensions")]
	pub extensions: Vec<String>,
	#[serde(rename = "Harmony")]
	pub harmony: Vec<String>
}

pub struct RuntimeData
{
	pub started: Instant,
}

#[derive(Default, Copy, Clone)]
pub struct MethodResult
{
	pub calls: u64,
	pub total_allocations: u64,
	pub own_allocations: u64,
	pub total_exceptions: u64,
	pub own_exceptions: u64,
	pub total_time: Duration,
	pub own_time: Duration
}

impl MethodResult
{
	#[inline(always)]
	pub fn merge_from(&mut self, input: &MethodResult)
	{
		self.total_time += input.total_time;
		self.own_time += input.own_time;
		self.calls += input.calls;
		self.total_allocations += input.total_allocations;
		self.own_allocations += input.own_allocations;
		self.total_exceptions += input.total_exceptions;
		self.own_exceptions += input.own_exceptions;
	}
}

#[derive(Default, Copy, Clone)]
pub struct MemoryResult
{
	pub allocations: u64,
	pub total_alloc_size: u64,
	pub instance_size: u32
}

impl MemoryResult
{
	#[inline(always)]
	pub fn merge_from(&mut self, input: &MemoryResult)
	{
		self.allocations += input.allocations;
		self.total_alloc_size += input.total_alloc_size;
	}
}
#[derive(Default)]
pub struct PluginResults
{
	pub calls: u64,
	pub allocations: u64,
	pub total_time: Duration,
	pub exceptions: u64
}

pub fn load_config(path: &str) -> anyhow::Result<ProfilerConfig>
{
	anyhow::Ok(serde_json::from_slice(fs::read(path)?.as_slice())?)
}

pub fn get_default_config(path: &str) -> ProfilerConfig
{
	let cfg = ProfilerConfig {
		instrument_methods: true,
		..Default::default()	
	};
	let _ = fs::write(path, serde_json::to_string_pretty(&cfg).unwrap().as_bytes());
	cfg
}

#[no_mangle]
pub unsafe extern "system" fn init_profiler(cfg_ptr: *const u16, cfg_len: i32)
{
	run_ffi_safe!("init_profiler", (), {

		let pstr = String::from_utf16(slice::from_raw_parts(cfg_ptr, cfg_len as usize)).unwrap();
		let cfg = match load_config(pstr.as_str())
		{
			Ok(x) => x,
			Err(_) => get_default_config(pstr.as_str())
		};
		
		if !cfg.enabled {return;}
		
		PROFILER = Some(MonoProfiler {
			main_domain: mono_domain_get(),
			callbacks: None,
			profiler_recording: false,
			args: ProfilerArgs::None,
			handle: ptr::null(),
			sync: RwLock::new(ProfilerSynced {
				config: cfg,
				profiler_images: Default::default(),
				profiler_method_image_map: Default::default(),
				profiler_method_detour_map: Default::default(),
				runtime: None,
			}),
			mono_defaults: MonoDefaults::new(),
			threads: Mutex::new(Vec::new())
		});
		
		#[cfg(debug_assertions)]
		{
			println!("gc incremental: {}", mono_gc_is_incremental());
		}
		
		get_thread_data().main = true;
		
		let mp = PROFILER.as_mut().unwrap_unchecked();

		mp.handle = mono_profiler_create(mp);

		let sync = mp.sync.get_mut().unwrap();
		
		mono_profiler_enable_allocations();
		
		if sync.config.instrument_methods {
			mono_profiler_set_call_instrumentation_filter_callback(mp.handle, Some(can_profile_method));
			mono_profiler_set_image_loaded_callback(mp.handle, Some(image_loaded));
		}
		#[cfg(debug_assertions)]
		{
			mono_profiler_set_method_free_callback(mp.handle, Some(method_free));
			println!("mono profiler started {:2x}", mp.handle as usize);
		}
		
		return;
	})
}

#[no_mangle]
pub unsafe extern "system" fn profiler_is_enabled() -> bool
{
	PROFILER.is_some()
}

pub const INST_FLAGS: CallInstrumentationFlags = 
	CallInstrumentationFlags::from_bits_retain(
		CallInstrumentationFlags::Enter.bits()
			| CallInstrumentationFlags::Leave.bits()
 			//| CallInstrumentationFlags::TailCall.bits()
			| CallInstrumentationFlags::ExceptionLeave.bits());

#[no_mangle]
pub unsafe extern "system" fn register_profiler_assembly(image_raw: *const MonoImage)
{
	let profiler = some_or_ret!(&PROFILER, ());
	let mut sync = ok_or_ret!(profiler.sync.write(), ());
	if image_raw.is_null() {return;}
	let image = &*image_raw;
	if is_image_blacklisted(image) {return}
	#[cfg(debug_assertions)]
	{
		println!("registering assembly from managed: {}", get_assembly_name(image));
	}
	sync.profiler_images.insert(image);
}

pub const IMAGE_BLACKLIST: &[&str] = &["UnityEngine.CoreModule"];

pub unsafe fn is_image_blacklisted(image: &MonoImage) -> bool
{
	if let Some(name) = read_cstr(image.assembly_name)
	{
		IMAGE_BLACKLIST.iter().any(|bl|
		{
			(*bl).eq(name)
		})
	}
	else { true }
}

pub unsafe extern "C" fn image_loaded(profiler: &MonoProfiler, image: &MonoImage)
{
	if is_image_blacklisted(image) {return}
	
	let td = get_thread_data();

	assert_main_domain!(profiler, td, ());

	let mut sync = ok_or_ret!(profiler.sync.write(), ());
	
	let name = read_cstr(image.assembly_name);
	
	if sync.config.assemblies.iter().any(|cfg_name|{
		if cfg_name.eq("*") {return true;}
		match name {
			None => false,
			Some(asmname) => asmname.eq(cfg_name)
		}
	})
	{
		sync.profiler_images.insert(image);
	}
}

pub unsafe fn get_assembly_name(profiler: &MonoImage) -> String
{
	read_cstr_default(profiler.assembly_name, ||{
		format!("__UNKNOWN: {:x}", transmute::<&MonoImage, usize>(profiler))
	})
}

#[derive(Default)]
#[repr(C)]
pub struct GCRecord
{
	pub calls: u64,
	pub total_time: u64,
}

#[repr(C)]
pub struct BasicRecord
{
	pub assembly_handle: *const MonoImage,
	pub total_time: u64,
	pub total_time_percentage: f64,
	pub total_exceptions: u64,
	pub calls: u64,
	pub alloc: u64,
}

impl Default for BasicRecord
{
	fn default() -> Self {
		Self
		{
			assembly_handle: ptr::null(),
			total_time: 0,
			total_time_percentage: 0f64,
			total_exceptions: 0,
			calls: 0,
			alloc: 0
		}
	}
}

#[repr(C)]
pub struct AdvancedRecord
{
	pub assembly_handle: *const MonoImage,
	pub method_handle: *const MonoMethod,
	pub total_time: u64,
	pub total_time_percentage: f64,
	pub own_time: u64,
	pub own_time_percentage: f64,
	pub calls: u64,
	pub total_alloc: u64,
	pub own_alloc: u64,
	pub total_exceptions: u64,
	pub own_exceptions: u64,
}

impl Default for AdvancedRecord
{
	fn default() -> Self {
		Self
		{
			assembly_handle: ptr::null(),
			method_handle: ptr::null(),
			total_time: 0,
			total_time_percentage: 0f64,
			own_time: 0,
			own_time_percentage: 0f64,
			calls: 0,
			total_alloc: 0,
			own_alloc: 0,
			total_exceptions : 0,
			own_exceptions: 0
		}
	}
}

#[repr(C)]
pub struct MemoryRecord
{
	pub assembly_handle: *const MonoImage,
	pub class_handle: *const MonoClass,
	pub allocations: u64,
	pub total_alloc_size: u64,
	pub instance_size: u32,
	pub class_token: u32,
}

impl Default for MemoryRecord
{
	fn default() -> Self {
		Self
		{
			assembly_handle: ptr::null(),
			class_handle: ptr::null(),
			class_token: 0,
			allocations: 0,
			total_alloc_size: 0,
			instance_size: 0
		}
	}
}
pub type BasicIter<'a> = (Iter<'a, *const MonoImage, PluginResults>, f64);
pub type AdvIter<'a> = (Iter<'a, *const MonoMethod, MethodResult>, f64);
pub type MemoryIter<'a> = (Iter<'a, *const MonoClass, MemoryResult>);

#[no_mangle]
pub unsafe extern "system" fn get_image_name(managed: &mut *const MonoString, image_ptr: *const MonoImage)
{
	let profiler = some_or_ret!(&PROFILER, ());
	let callbacks = *some_or_ret!(&profiler.callbacks, ());
	let image = deref_or_ret!(image_ptr, ());
	if let Some(cs) = read_cstr((*image).assembly_name)
	{
		(callbacks.string_marshal)(managed, cs.as_ptr(), cs.len() as i32);
	}
}

#[no_mangle]
pub unsafe extern "system" fn get_method_name(managed: &mut *const MonoString, method_ptr: *const MonoMethod)
{
	let profiler = some_or_ret!(&PROFILER, ());
	let callbacks = *some_or_ret!(&profiler.callbacks, ());
	let method = deref_or_ret!(method_ptr, ());
	let full_name = get_method_full_name(method, MonoTypeNameFormat::FullName);
	(callbacks.string_marshal)(managed, full_name.as_ptr(), full_name.len() as i32);
}

#[no_mangle]
pub unsafe extern "system" fn get_class_name(managed: &mut *const MonoString, class_ptr: *const MonoClass)
{
	let profiler = some_or_ret!(&PROFILER, ());
	let callbacks = *some_or_ret!(&profiler.callbacks, ());
	let class = deref_or_ret!(class_ptr, ());
	let nameptr = mono_type_get_name_full(&class._byval_arg, MonoTypeNameFormat::FullName);
	let full_name = CStr::from_ptr(nameptr).to_str().unwrap();
	(callbacks.string_marshal)(managed, full_name.as_ptr(), full_name.len() as i32);
	mono_free_g(nameptr);
}

#[no_mangle]
pub unsafe extern "system" fn profiler_register_callbacks(callbacks: &ManagedCallbacks)
{
	let profiler = some_or_ret!(&mut PROFILER, ());
	profiler.callbacks = Some(*callbacks);
}

bitflags! {
	#[repr(C)]
	#[derive(PartialEq, Copy, Clone)]
	pub struct ProfilerArgs: u16
	{
		const None = 0;
		const Abort = 1 << 0;
		const CallMemory = 1 << 1;
		const AdvancedMemory = 1 << 2;
		const Timings = 1 << 3;
		const Stack = 1 << 4;
		const FastResume = 1 << 5;
		const GCEvent = 1 << 6;
		//const ExportProto = 1 << 5;  // TODO: add exports
		//const ExportJson = 1 << 6;
	}
}

#[no_mangle]
pub unsafe extern "system" fn profiler_toggle(
	mut args: ProfilerArgs,
	state: &mut bool,
	gc_out: *mut GCRecord,
	basic_out: *mut (),
	adv_out: *mut (),
	mem_out: *mut ()
) -> ProfilerResultCode
{
	return run_ffi_safe!("profiler_toggle", ProfilerResultCode::UnknownError, {
		let td = get_thread_data();
		if !td.main {return ProfilerResultCode::MainThreadOnly;}
		let profiler = some_or_ret!(&mut PROFILER, ProfilerResultCode::NotInitialized);
		let mut others = ok_or_ret!(profiler.threads.lock(), ProfilerResultCode::CorruptedState);
		let mut sync = ok_or_ret!(profiler.sync.write(), ProfilerResultCode::CorruptedState);
		let callbacks = *some_or_ret!(&profiler.callbacks, ProfilerResultCode::NotInitialized);
		match !profiler.profiler_recording {
			true => {
				if args.contains(ProfilerArgs::Abort)
				{
					return ProfilerResultCode::Aborted;
				}
				
				if profiler.args & ProfilerArgs::FastResume != args & ProfilerArgs::FastResume
				{
					if profiler.args.contains(ProfilerArgs::FastResume) {
						for thread in others.iter_mut()
						{
							thread.stack.clear();
						}
					}
					else
					{
						args.remove(ProfilerArgs::FastResume);
					}
				}
				
				if !args.contains(ProfilerArgs::FastResume) {
					let mut any = false;
					
					let stack = sync.config.instrument_methods && args.contains(ProfilerArgs::Stack);
					
					let call_memory = stack && args.contains(ProfilerArgs::CallMemory);
					
					if stack
					{
						any = true;
						let enter: EnterCB;
						let exit: ExitCB;
						let exception: ExceptionalExitCB;
						if args.contains(ProfilerArgs::Timings) {
							enter = method_enter::<true>;
							exit = method_leave::<true>;
							exception = method_exception_leave::<true>;
						}
						else
						{
							enter = method_enter::<false>;
							exit = method_leave::<false>;
							exception = method_exception_leave::<false>;
						}
						mono_profiler_set_method_enter_callback(profiler.handle, Some(enter));
						mono_profiler_set_method_leave_callback(profiler.handle, Some(exit));
						mono_profiler_set_method_exception_leave_callback(profiler.handle, Some(exception));
						mono_profiler_set_exception_throw_callback(profiler.handle, Some(exception_thrown));
					}
					else
					{
						args.remove(ProfilerArgs::CallMemory);
					}
					
					if call_memory || args.contains(ProfilerArgs::AdvancedMemory) {
						any = true;
						
						let alloc_cb: GCAllocCB;
						
						if call_memory && args.contains(ProfilerArgs::AdvancedMemory)
						{
							alloc_cb = gc_alloc_cb::<true, true>;
						}
						else if call_memory
						{
							alloc_cb = gc_alloc_cb::<true, false>;
						}
						else // Only advanced memory
						{
							alloc_cb = gc_alloc_cb::<false, true>;
						}
						
						mono_profiler_set_gc_allocation_callback(profiler.handle, Some(alloc_cb));
					}
					
					if args.contains(ProfilerArgs::GCEvent)
					{
						any = true;
						mono_profiler_set_gc_event_callback(profiler.handle, Some(gc_collect_cb));
					}
					
					if !any
					{
						*state = false;
						return ProfilerResultCode::InvalidArgs;
					}
					
					profiler.args = args;
				}
				
				sync.runtime = Some(RuntimeData
				{
					started: Instant::now()
				});
				
				profiler.profiler_recording = true;
				*state = true;
				drop(sync);
				drop(others);
			}
			false => {
				profiler.profiler_recording = false;
				if args.contains(ProfilerArgs::FastResume) && !args.contains(ProfilerArgs::Abort) {
					profiler.args = args;
				}
				else
				{
					profiler.args = ProfilerArgs::None;
					mono_profiler_set_method_enter_callback(profiler.handle, None);
					mono_profiler_set_method_leave_callback(profiler.handle, None);
					mono_profiler_set_method_exception_leave_callback(profiler.handle, None);
					mono_profiler_set_gc_allocation_callback(profiler.handle, None);
					mono_profiler_set_gc_event_callback(profiler.handle, None);
					mono_profiler_set_exception_throw_callback(profiler.handle, None);
				}
				//println!("Profile complete");
				let now = Instant::now();
				// TODO: do this the right way
				thread::sleep(Duration::from_millis(25));

				let rt = mem::take(&mut sync.runtime).unwrap();
				
				if args.contains(ProfilerArgs::Abort)
				{
					for thread in others.iter_mut() {
						thread.call_recording.clear();
						thread.memory_recording.clear();
						thread.stack.clear();
						thread.gc_timings = GCTimings::default();
					}
					return ProfilerResultCode::Aborted;
				}
				
				let mut call_data = mem::take(&mut td.call_recording);
				
				let mut mem_data = mem::take(&mut td.memory_recording);
				
				if args.contains(ProfilerArgs::GCEvent) && !gc_out.is_null()
				{
					let gct = mem::take(&mut td.gc_timings);
					*gc_out = GCRecord
					{
						calls: gct.calls,
						total_time: gct.total_time.as_micros() as u64
					}
				}
				
				for thread in others.iter_mut() {
					if !args.contains(ProfilerArgs::FastResume) {
						thread.stack.clear();
					}
					if ptr::eq(*thread, td) {continue;}
					for (method, result) in &mut thread.call_recording {
						match call_data.entry(*method) {
							Entry::Occupied(mut x) => x.get_mut().merge_from(result),
							Entry::Vacant(x) => {x.insert(*result);}
						}
					}
					for (klass, result) in &mut thread.memory_recording {
						match mem_data.entry(*klass) {
							Entry::Occupied(mut current) => current.get_mut().merge_from(result),
							Entry::Vacant(x) => {x.insert(*result);}
						}
					}
					thread.call_recording.clear();
					thread.memory_recording.clear();
				}

				let total_time = now.duration_since(rt.started).as_millis() as f64;
				
				let mut plugin_data: HashMap<*const MonoImage, PluginResults> = HashMap::with_capacity(sync.profiler_images.len());
				for (method, method_results) in call_data.iter_mut() {
					let entry = plugin_data.entry((*(**method).class).image).or_default();
					entry.calls += method_results.calls;
					entry.allocations += method_results.own_allocations;
					entry.total_time += method_results.own_time;
					entry.exceptions += method_results.own_exceptions;
				}
				let mut basic_ret: BasicIter = (plugin_data.iter(), total_time);

				*state = false;
				
				// managed methods cannot be called while holding the profiler lock
				
				drop(sync);
				drop(others);
				
				if !plugin_data.is_empty()
				{
					(callbacks.basic_iter)(basic_out, plugin_data.len() as u64, &mut basic_ret, iter_basic_fn);
				}
				
				if !call_data.is_empty()
				{
					(callbacks.adv_iter)(adv_out, call_data.len() as u64, &mut (call_data.iter(), total_time), iter_advanced_fn);
				}
				
				if !mem_data.is_empty()
				{
					(callbacks.mem_iter)(mem_out, mem_data.len() as u64, &mut (mem_data.iter()), iter_memory_fn);
				}
				
				call_data.clear();
				td.call_recording = call_data;
				mem_data.clear();
				td.memory_recording = mem_data;
			}
		}

		ProfilerResultCode::OK
	});
	unsafe extern "system" fn iter_basic_fn(
		(iter, total_time): &mut BasicIter,
		out: &mut BasicRecord
	) -> bool
	{
		if let Some((image, result)) = iter.next()
		{
			*out = BasicRecord
			{
				assembly_handle: *image,
				total_time: result.total_time.as_micros() as u64,
				total_time_percentage: (result.total_time.as_millis() as f64 / *total_time) * 100f64,
				calls: result.calls,
				alloc: result.allocations,
				total_exceptions: result.exceptions
			};
			return true;
		}
		false
	}
	unsafe extern "system" fn iter_advanced_fn(
		(iter, total_time): &mut AdvIter,
		out: &mut AdvancedRecord
	) -> bool
	{
		if let Some((method, result)) = iter.next()
		{
			*out = AdvancedRecord
			{
				assembly_handle: (*(*(*method)).class).image,
				method_handle: *method,
				total_time: result.total_time.as_micros() as u64,
				total_time_percentage: (result.total_time.as_millis() as f64 / *total_time) * 100f64,
				own_time: result.own_time.as_micros() as u64,
				own_time_percentage: (result.own_time.as_millis() as f64 / *total_time) * 100f64,
				calls: result.calls,
				total_alloc: result.total_allocations,
				own_alloc: result.own_allocations,
				total_exceptions: result.total_exceptions,
				own_exceptions: result.own_exceptions
			};
			return true;
		}
		false
	}

	unsafe extern "system" fn iter_memory_fn(
		(iter): &mut MemoryIter,
		out: &mut MemoryRecord
	) -> bool
	{
		if let Some((class_ptr, result)) = iter.next()
		{
			let class = &**class_ptr;
			*out = MemoryRecord
			{
				assembly_handle: class.image,
				class_token: class.type_token,
				class_handle: *class_ptr,
				allocations: result.allocations,
				total_alloc_size: result.total_alloc_size,
				instance_size: class.instance_size as u32
			};
			return true;
		}
		false
	}
}


pub unsafe fn debug_log_method(method: &MonoMethod)
{
	println!("name: {}", method.get_name());
	let klass = match !method.class.is_null() {
		true => &*method.class,
		false => {println!("klass null");return;}
	};
	println!("class: {}", CStr::from_ptr(klass.name).to_str().unwrap());
	let image = match !klass.image.is_null() {
		true => &*klass.image,
		false => {println!("image null");return;}
	};
	if image.name.is_null() { print!("image name null");return;}
	println!("image: {}", CStr::from_ptr(image.name).to_str().unwrap());
}

#[cfg(debug_assertions)]
pub unsafe extern "C" fn method_free(_profiler: &MonoProfiler, method: &MonoMethod)
{
	debug_log_method(method);
	println!("method free");
}

pub unsafe extern "C" fn can_profile_method(profiler: &MonoProfiler, mut method: &MonoMethod) -> CallInstrumentationFlags
{
	let td = get_thread_data();
	assert_main_domain!(profiler, td, CallInstrumentationFlags::None);
	let sync = ok_or_ret!(profiler.sync.read(), CallInstrumentationFlags::None);
	match sync.profiler_method_detour_map.get(&(method as *const MonoMethod))
	{
		None => {}
		Some(x) => {
			method = &**x;
		}
	}
	if method.class.is_null() || (*method.class).image.is_null() {return CallInstrumentationFlags::None;}
	let asm: *const MonoImage = (*method.class).image;
	if sync.profiler_images.contains(&asm)
	{
		//debug_log_method(method);
		INST_FLAGS
	}
	else {
		//debug_log_method(method);
		CallInstrumentationFlags::None
	}
}


pub unsafe fn get_sizeof_object(profiler: &MonoProfiler, object: &MonoObject) -> usize
{
	let klass = &(*(*object.vtable).klass);

	match klass.class_kind {
		MonoTypeKind::Def | MonoTypeKind::Gtd | MonoTypeKind::Ginst => {
			if ptr::eq(klass, profiler.mono_defaults.string_class)
			{
				let mstr: &MonoString = transmute(object);
				return klass.instance_size as usize + (mstr.length as usize * 2)
			}
			klass.instance_size as usize
		}
		MonoTypeKind::Array => {
			let arr: &MonoArray = transmute(object);
			klass.instance_size as usize + (arr.length as usize * klass.sizes.element_size as usize)
		}
		_ => {0}
	}
}
#[derive(Default)]
pub struct GCTimings
{
	pub total_time: Duration,
	pub calls: u64
}

pub unsafe extern "C" fn gc_collect_cb(profiler: &MonoProfiler, event: MonoProfilerGCEvent, /* always 0 - true in boehm */ _generation: u32, _serial: bool)
{
	if !profiler.profiler_recording {return;}
	let td = get_thread_data();
	
	if !td.main {return;}

	// https://github.com/Unity-Technologies/mono/blob/1c8577ecab30c96474982a3bcfaf4ab2d84fef7a/mono/metadata/boehm-gc.c#L556
	// https://github.com/ivmai/bdwgc/blob/67090a19904291db97ec3a76eb6ae420f3564cea/alloc.c#L825
	match event {
		// GC start
		MonoProfilerGCEvent::PreStopWorld => {
			td.gc_clock = Some(Instant::now());
		},
		// GC end
		MonoProfilerGCEvent::PostStartWorldUnlocked => {
			if let Some(gct) = td.gc_clock
			{
				let now = Instant::now();
				let time = now.duration_since(gct);
				td.gc_timings.calls += 1;
				td.gc_timings.total_time += time;
				#[cfg(debug_assertions)]
				{
					println!("gc time: {} - {} -------------------------------------", time.as_micros(), td.main)
				}
			}
		},
		_ => {}
	}
}

pub unsafe extern "C" fn gc_alloc_cb<const CALL: bool, const ADV: bool>(profiler: &MonoProfiler, object: &MonoObject)
{
	if !profiler.profiler_recording {return;}
	let td = get_thread_data();
	assert_main_domain!(profiler, td, ());
	let alloc = get_sizeof_object(profiler, object) as u64;
	if CALL {
		if let Some(frame) = td.stack.last_mut()
		{
			frame.own_allocations += alloc;
			frame.total_allocations += alloc;
		};
	}
	if ADV {
		let klass = (*object.vtable).klass;
		if klass.is_null() {return;}
		let entry = td.memory_recording.entry(klass).or_insert_with(||{
			MemoryResult
			{
				instance_size: (*klass).instance_size as u32,
				..Default::default()
			}
		});
		entry.allocations += 1;
		entry.total_alloc_size += alloc;
	}
}

#[inline]
pub unsafe extern "C" fn method_enter<const TIMED: bool>(profiler: &MonoProfiler, method: &MonoMethod, _ctx: *const MonoProfilerCallContext)
{
	if !profiler.profiler_recording { return; }
	let td = get_thread_data();
	assert_main_domain!(profiler, td, ());
	td.stack.push(StackFrame::new(method, if TIMED && td.main
	{
		Some(StackTiming
		{
			time: Instant::now(),
			others: Duration::ZERO
		})
	}
	else {
		None
	}));
}

pub unsafe extern "C" fn exception_thrown(profiler: &MonoProfiler, _obj: &MonoObject)
{
	if !profiler.profiler_recording { return; }

	let td = get_thread_data();

	if let Some(frame) = td.stack.last_mut()
	{
		#[cfg(debug_assertions)]
		{
			println!("root exception in {}", (*frame.method).get_name());
		}
		let result: &mut MethodResult = td.call_recording.entry(frame.method).or_default();
		result.own_exceptions += 1;
	};
}

pub unsafe extern "C" fn method_leave<const TIMED: bool>(profiler: &MonoProfiler, method_ptr: *const MonoMethod, _ctx: *const MonoProfilerCallContext)
{
	exit_method::<TIMED>(profiler, method_ptr, false);
}
#[inline]
pub unsafe fn exit_method<const TIMED: bool>(profiler: &MonoProfiler, method_ptr: *const MonoMethod, exceptional: bool)
{
	if !profiler.profiler_recording { return; }

	let td = get_thread_data();

	assert_main_domain!(profiler, td, ());

	let idx: usize;
	let frame: &StackFrame = match td.stack.iter().rposition(|sf| {
		sf.method == method_ptr
	}) {
		None => {
			// stack invalid
			td.stack.clear();
			return;
		},
		Some(x) => {
			idx = x;
			td.stack.get_unchecked(x)
		}
	};

	let maybe_now: Option<Instant> = if TIMED && /* only some on main thread */ frame.time.is_some() {
		Some(Instant::now())
	}
	else {
		None
	};

	let result: &mut MethodResult = td.call_recording.entry(method_ptr).or_default();

	result.calls += 1;

	result.own_allocations += frame.own_allocations;

	let total_alloc = frame.total_allocations;

	result.total_allocations += total_alloc;
	
	if exceptional
	{
		result.total_exceptions += 1;
	}

	let mut total_time: Duration = Duration::ZERO;
	if TIMED {
		if let Some(now) = &maybe_now
		{
			let frame_time = frame.time.as_ref().unwrap_unchecked();
			total_time = now.duration_since(frame_time.time);
			result.total_time += total_time;
			result.own_time += total_time - frame_time.others;
		}
	}

	if let Some(prev_idx) = idx.checked_sub(1)
	{
		let prev = td.stack.get_unchecked_mut(prev_idx);
		prev.total_allocations += total_alloc;
		if TIMED {
			if let Some(prev_time) = &mut prev.time
			{
				prev_time.others += total_time;
			}
		}
	}

	td.stack.truncate(idx);
}

pub unsafe extern "C" fn method_exception_leave<const TIMED: bool>(profiler: &MonoProfiler, method: *const MonoMethod, _exception: *const MonoObject)
{
	#[cfg(debug_assertions)]
	{
		println!("exception in {}", (*method).get_name());
	}
	exit_method::<TIMED>(profiler, method, true);
}