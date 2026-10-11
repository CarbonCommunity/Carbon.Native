/*
 *
 * Copyright (c) 2023-2024 Patrette, under the GNU v3 license rights
 * Copyright (c) 2023-2024 Carbon Community, under the GNU v3 license rights
 *
 */

use bitflags::bitflags;
use serde::{Deserialize, Serialize};
use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::{Cell, OnceCell, RefCell, UnsafeCell};
use std::collections::hash_map::Iter;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU16, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock, PoisonError};
use std::time::{Duration, Instant};
use std::{fs, io, mem, ptr, slice, thread};
use crate::mono::*;
use crate::recording::{percentage_of, AllocationScopes, CallRecorder, MemoryRecorder, MemoryResult, MethodResult};
use crate::{log_mono_native_host, ok_or_ret, read_cstr, read_cstr_default, run_ffi_safe, some_or_ret, Severity};

pub static PROFILER: OnceLock<MonoProfiler> = OnceLock::new();

thread_local! {
	static THREAD_STATE: ThreadState = const {
		ThreadState {
			slot: OnceCell::new(),
			domain: Cell::new(ptr::null()),
			allocation_scopes: RefCell::new(AllocationScopes::new())
		}
	};
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
	Busy = 7,
	NoOp = 8
}

struct ThreadState
{
	slot: OnceCell<Arc<ThreadSlot>>,
	domain: Cell<*const MonoDomain>,
	allocation_scopes: RefCell<AllocationScopes>
}

impl ThreadState
{
	fn domain(&self) -> *const MonoDomain
	{
		if self.domain.get().is_null() {
			self.domain.set(unsafe { mono_domain_get() });
		}
		self.domain.get()
	}
}

impl Drop for ThreadState
{
	fn drop(&mut self)
	{
		if let Some(slot) = self.slot.get() {
			slot.retired.store(true, Ordering::Release);
		}
	}
}

#[derive(Default)]
pub struct ThreadRecording
{
	pub calls: CallRecorder,
	pub memory: MemoryRecorder
}

pub struct ThreadSlot
{
	main: bool,
	busy: AtomicBool,
	retired: AtomicBool,
	recording: UnsafeCell<ThreadRecording>
}

unsafe impl Send for ThreadSlot {}
unsafe impl Sync for ThreadSlot {}

impl ThreadSlot
{
	fn new(main: bool) -> Self
	{
		ThreadSlot {
			main,
			busy: AtomicBool::new(false),
			retired: AtomicBool::new(false),
			recording: UnsafeCell::new(ThreadRecording::default())
		}
	}

	fn wait_until_idle(&self)
	{
		while self.busy.load(Ordering::SeqCst) {
			thread::yield_now();
		}
	}

	unsafe fn edit_recording<R>(&self, edit: impl FnOnce(&mut ThreadRecording) -> R) -> R
	{
		edit(&mut *self.recording.get())
	}
}

fn record(profiler: &MonoProfiler, edit: impl FnOnce(&mut ThreadRecording, bool))
{
	if !profiler.is_recording() {
		return;
	}
	let _ = THREAD_STATE.try_with(|state| {
		let slot = state.slot.get_or_init(|| profiler.register_thread(false));
		if state.domain() != profiler.main_domain {
			return;
		}
		// Pairs with the SeqCst store in profiler_toggle: either this thread sees profiler_recording == false,
		// or profiler_toggle sees busy == true and waits before it reads this thread's recording.
		slot.busy.store(true, Ordering::SeqCst);
		if profiler.profiler_recording.load(Ordering::SeqCst) {
			unsafe { slot.edit_recording(|recording| edit(recording, slot.main)) };
		}
		slot.busy.store(false, Ordering::Release);
	});
}

fn is_main_thread() -> bool
{
	THREAD_STATE.try_with(|state| state.slot.get().is_some_and(|slot| slot.main)).unwrap_or(false)
}

fn with_allocation_scopes<R>(edit: impl FnOnce(&mut AllocationScopes) -> R) -> Option<R>
{
	THREAD_STATE.try_with(|state| {
		state.allocation_scopes.try_borrow_mut().ok().map(|mut scopes| edit(&mut scopes))
	}).ok().flatten()
}

pub struct MonoProfiler
{
	pub profiler_recording: AtomicBool,
	pub main_domain: *const MonoDomain,
	pub args: AtomicU16,
	pub gc_alloc_stack_tracking: AtomicBool,
	pub handle: AtomicPtr<()>,
	pub sync: Mutex<ProfilerSynced>,
	pub mono_defaults: MonoDefaults,
	pub threads: Mutex<Vec<Arc<ThreadSlot>>>,
	pub callbacks: Mutex<Option<ManagedCallbacks>>,
	pub freed_methods: Mutex<FxHashSet<*const MonoMethod>>,
	pub epoch: Instant,
	pub gc: GcClock,
}

unsafe impl Send for MonoProfiler {}
unsafe impl Sync for MonoProfiler {}

impl MonoProfiler
{
	fn is_recording(&self) -> bool
	{
		self.profiler_recording.load(Ordering::Relaxed)
	}

	fn args(&self) -> ProfilerArgs
	{
		ProfilerArgs::from_bits_retain(self.args.load(Ordering::Relaxed))
	}

	fn set_args(&self, args: ProfilerArgs)
	{
		self.args.store(args.bits(), Ordering::Relaxed);
	}

	fn handle(&self) -> MonoProfilerHandle
	{
		self.handle.load(Ordering::Relaxed)
	}

	fn callbacks(&self) -> Option<ManagedCallbacks>
	{
		self.callbacks.lock().ok().and_then(|callbacks| *callbacks)
	}

	fn elapsed_nanos(&self) -> u64
	{
		self.epoch.elapsed().as_nanos() as u64
	}

	fn register_thread(&self, main: bool) -> Arc<ThreadSlot>
	{
		let slot = Arc::new(ThreadSlot::new(main));
		self.threads.lock().unwrap_or_else(PoisonError::into_inner).push(slot.clone());
		slot
	}
}

pub struct ProfilerSynced
{
	pub config: ProfilerConfig,
	pub profiler_images: FxHashSet<*const MonoImage>,
	pub runtime: Option<RuntimeData>,
}

pub type BasicIterCallback = extern "system" fn(*mut (), u64, &mut BasicIter, unsafe extern "system" fn(&mut BasicIter, &mut BasicRecord) -> bool);
pub type AdvancedIterCallback = extern "system" fn(*mut (), u64, &mut AdvIter, unsafe extern "system" fn(&mut AdvIter, &mut AdvancedRecord) -> bool);
pub type MemoryIterCallback = extern "system" fn(*mut (), u64, &mut MemoryIter, unsafe extern "system" fn(&mut MemoryIter, &mut MemoryRecord) -> bool);

pub type ManagedStringMarshalFunc = extern "system" fn(&mut *const MonoString, *const u8, i32);
pub type ManagedArrMarshalFunc<T> = extern "system" fn(&mut *const MonoArray, *const T, u64);

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

#[derive(Serialize, Deserialize)]
#[serde(default)]
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

impl Default for ProfilerConfig
{
	fn default() -> Self
	{
		ProfilerConfig {
			enabled: false,
			instrument_methods: true,
			sourceviewer: false,
			assemblies: Vec::new(),
			plugins: Vec::new(),
			modules: Vec::new(),
			extensions: Vec::new(),
			harmony: Vec::new()
		}
	}
}

pub struct RuntimeData
{
	pub started: Instant,
}

#[derive(Default)]
pub struct PluginResults
{
	pub calls: u64,
	pub allocations: u64,
	pub total_time: Duration,
	pub exceptions: u64
}

pub fn load_config(path: &str) -> anyhow::Result<Option<ProfilerConfig>>
{
	match fs::read(path) {
		Ok(bytes) => Ok(Some(serde_json::from_slice(&bytes)?)),
		Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(None),
		Err(error) => Err(error.into())
	}
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
		if PROFILER.get().is_some() {
			log_mono_native_host(Severity::Warning, "init_profiler was called again; the profiler is already initialized");
			return;
		}

		let pstr = String::from_utf16_lossy(slice::from_raw_parts(cfg_ptr, cfg_len as usize));
		let cfg = match load_config(pstr.as_str())
		{
			Ok(Some(x)) => x,
			Ok(None) => get_default_config(pstr.as_str()),
			Err(error) => {
				log_mono_native_host(Severity::Error, &format!("Profiler disabled, cannot read {pstr}: {error}"));
				return;
			}
		};

		if !cfg.enabled {return;}

		//init_detour();

		let instrument_methods = cfg.instrument_methods;

		let mp = PROFILER.get_or_init(|| MonoProfiler {
			main_domain: mono_domain_get(),
			gc_alloc_stack_tracking: AtomicBool::new(false),
			callbacks: Mutex::new(None),
			profiler_recording: AtomicBool::new(false),
			args: AtomicU16::new(ProfilerArgs::None.bits()),
			handle: AtomicPtr::new(ptr::null_mut()),
			sync: Mutex::new(ProfilerSynced {
				config: cfg,
				profiler_images: Default::default(),
				runtime: None,
			}),
			mono_defaults: MonoDefaults::new(),
			threads: Mutex::new(Vec::new()),
			freed_methods: Mutex::new(FxHashSet::default()),
			epoch: Instant::now(),
			gc: GcClock::default()
		});

		#[cfg(debug_assertions)]
		{
			println!("gc incremental: {}", mono_gc_is_incremental());
		}

		THREAD_STATE.with(|state| {
			state.slot.get_or_init(|| mp.register_thread(true));
		});

		let handle = mono_profiler_create(ptr::from_ref(mp).cast_mut());
		mp.handle.store(handle.cast_mut(), Ordering::Relaxed);

		mono_profiler_enable_allocations();
		mono_profiler_set_method_free_callback(handle, Some(method_free));

		if instrument_methods {
			mono_profiler_set_call_instrumentation_filter_callback(handle, Some(can_profile_method));
			mono_profiler_set_image_loaded_callback(handle, Some(image_loaded));
		}
		#[cfg(debug_assertions)]
		{
			println!("mono profiler started {:2x}", handle as usize);
		}

		return;
	})
}

#[no_mangle]
pub unsafe extern "system" fn profiler_is_enabled() -> bool
{
	PROFILER.get().is_some()
}

pub const INST_FLAGS: CallInstrumentationFlags =
	CallInstrumentationFlags::from_bits_retain(
		CallInstrumentationFlags::Enter.bits()
			| CallInstrumentationFlags::Leave.bits()
			//| CallInstrumentationFlags::TailCall.bits()
			| CallInstrumentationFlags::ExceptionLeave.bits());

#[no_mangle]
pub unsafe extern "system" fn register_profiler_assembly(image: *const MonoImage)
{
	let profiler = some_or_ret!(PROFILER.get(), ());
	let mut sync = ok_or_ret!(profiler.sync.lock(), ());
	if image.is_null() {return;}
	if is_image_blacklisted(image) {return}
	#[cfg(debug_assertions)]
	{
		println!("registering assembly from managed: {}", get_assembly_name(image));
	}
	sync.profiler_images.insert(image);
}

pub const IMAGE_BLACKLIST: &[&str] = &["UnityEngine.CoreModule"];

pub unsafe fn is_image_blacklisted(image: *const MonoImage) -> bool
{
	if let Some(name) = read_cstr(mono_image_get_name(image))
	{
		IMAGE_BLACKLIST.iter().any(|bl|
			{
				bl.as_bytes() == name.to_bytes()
			})
	}
	else { true }
}

pub unsafe extern "C" fn image_loaded(profiler: &MonoProfiler, image: *const MonoImage)
{
	if is_image_blacklisted(image) {return}

	if mono_domain_get() != profiler.main_domain {return}

	let mut sync = ok_or_ret!(profiler.sync.lock(), ());

	let name = read_cstr(mono_image_get_name(image));

	if sync.config.assemblies.iter().any(|cfg_name|{
		if cfg_name.eq("*") {return true;}
		match name {
			None => false,
			Some(asmname) => asmname.to_bytes() == cfg_name.as_bytes()
		}
	})
	{
		sync.profiler_images.insert(image);
	}
}

pub unsafe fn get_assembly_name(profiler: *const MonoImage) -> String
{
	read_cstr_default(mono_image_get_name(profiler), ||{
		format!("__UNKNOWN: {:x}", profiler as usize)
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
pub type BasicIter<'a> = (Iter<'a, *const MonoImage, PluginResults>, Duration);
pub type AdvIter<'a> = (Iter<'a, *const MonoMethod, MethodResult>, Duration);
pub type MemoryIter<'a> = Iter<'a, *const MonoClass, MemoryResult>;

#[no_mangle]
pub unsafe extern "system" fn get_image_name(managed: &mut *const MonoString, image_ptr: *const MonoImage)
{
	let callbacks = some_or_ret!(PROFILER.get().and_then(MonoProfiler::callbacks), ());
	if image_ptr.is_null() {return;}
	if let Some(cs) = read_cstr(mono_image_get_name(image_ptr))
	{
		let cs = cs.to_bytes();
		(callbacks.string_marshal)(managed, cs.as_ptr(), cs.len() as i32);
	}
}

#[no_mangle]
pub unsafe extern "system" fn get_method_name(managed: &mut *const MonoString, method_ptr: *const MonoMethod)
{
	let callbacks = some_or_ret!(PROFILER.get().and_then(MonoProfiler::callbacks), ());
	if method_ptr.is_null() {return;}
	let full_name = get_method_full_name(method_ptr, MonoTypeNameFormat::FullName);
	(callbacks.string_marshal)(managed, full_name.as_ptr(), full_name.len() as i32);
}

#[no_mangle]
pub unsafe extern "system" fn get_class_name(managed: &mut *const MonoString, class_ptr: *const MonoClass)
{
	let callbacks = some_or_ret!(PROFILER.get().and_then(MonoProfiler::callbacks), ());
	if class_ptr.is_null() {return;}
	let nameptr = mono_type_get_name_full(mono_class_get_type(class_ptr), MonoTypeNameFormat::FullName);
	if let Some(full_name) = read_cstr(nameptr)
	{
		let full_name = full_name.to_bytes();
		(callbacks.string_marshal)(managed, full_name.as_ptr(), full_name.len() as i32);
	}
	mono_free_g(nameptr);
}

#[no_mangle]
pub unsafe extern "system" fn profiler_register_callbacks(callbacks: &ManagedCallbacks)
{
	let profiler = some_or_ret!(PROFILER.get(), ());
	if let Ok(mut registered) = profiler.callbacks.lock() {
		*registered = Some(*callbacks);
	}
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
		const StackWalkAllocations = 1 << 7;
		//const ExportProto = 1 << 5;  // TODO: add exports
		//const ExportJson = 1 << 6;
	}
}

#[no_mangle]
pub unsafe extern "C" fn enable_method_alloc_stack(toggle: bool) -> ProfilerResultCode {
	let profiler = some_or_ret!(PROFILER.get(), ProfilerResultCode::NotInitialized);
	if !is_main_thread() {
		return ProfilerResultCode::MainThreadOnly;
	}
	if profiler.is_recording() {
		return ProfilerResultCode::Busy;
	}
	if profiler.gc_alloc_stack_tracking.load(Ordering::Relaxed) == toggle {
		return ProfilerResultCode::NoOp;
	}
	profiler.gc_alloc_stack_tracking.store(toggle, Ordering::Relaxed);
	mono_profiler_set_gc_allocation_callback(profiler.handle(), allocation_callback(false, false, toggle));
	ProfilerResultCode::OK
}
#[no_mangle]
pub unsafe extern "C" fn profiler_alloc_reset() {
	with_allocation_scopes(AllocationScopes::reset);
}
#[no_mangle]
pub unsafe extern "C" fn profiler_alloc_push() -> bool {
	with_allocation_scopes(AllocationScopes::push).unwrap_or(false)
}
#[no_mangle]
pub unsafe extern "C" fn profiler_alloc_pop() -> u64 {
	with_allocation_scopes(AllocationScopes::pop).unwrap_or_default()
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
		let profiler = some_or_ret!(PROFILER.get(), ProfilerResultCode::NotInitialized);
		if !is_main_thread() {return ProfilerResultCode::MainThreadOnly;}
		let mut others = ok_or_ret!(profiler.threads.lock(), ProfilerResultCode::CorruptedState);
		let mut sync = ok_or_ret!(profiler.sync.lock(), ProfilerResultCode::CorruptedState);
		let callbacks = some_or_ret!(profiler.callbacks(), ProfilerResultCode::NotInitialized);
		match !profiler.is_recording() {
			true => {
				if args.contains(ProfilerArgs::Abort)
				{
					return ProfilerResultCode::Aborted;
				}

				others.retain(|thread| !thread.retired.load(Ordering::Acquire));

				if profiler.args() & ProfilerArgs::FastResume != args & ProfilerArgs::FastResume
				{
					if profiler.args().contains(ProfilerArgs::FastResume) {
						for thread in others.iter()
						{
							thread.edit_recording(|recording| recording.calls.clear());
						}
					}
					else
					{
						args.remove(ProfilerArgs::FastResume);
					}
				}

				if !args.contains(ProfilerArgs::FastResume) {
					if !install_callbacks(profiler, args, sync.config.instrument_methods)
					{
						*state = false;
						return ProfilerResultCode::InvalidArgs;
					}

					profiler.set_args(args);
				}

				if let Ok(mut freed) = profiler.freed_methods.lock() {
					freed.clear();
				}
				profiler.gc.take();

				sync.runtime = Some(RuntimeData
				{
					started: Instant::now()
				});

				profiler.profiler_recording.store(true, Ordering::SeqCst);
				*state = true;
				drop(sync);
				drop(others);
			}
			false => {
				profiler.profiler_recording.store(false, Ordering::SeqCst);
				if args.contains(ProfilerArgs::FastResume) && !args.contains(ProfilerArgs::Abort) {
					profiler.set_args(args);
				}
				else
				{
					uninstall_callbacks(profiler);
					profiler.set_args(ProfilerArgs::None);
				}
				//println!("Profile complete");
				let now = Instant::now();
				for thread in others.iter() {
					thread.wait_until_idle();
				}

				let rt = mem::take(&mut sync.runtime).unwrap();
				let gc = profiler.gc.take();

				if args.contains(ProfilerArgs::Abort)
				{
					for thread in others.iter() {
						thread.edit_recording(|recording| {
							recording.calls.clear();
							recording.memory.clear();
						});
					}
					others.retain(|thread| !thread.retired.load(Ordering::Acquire));
					return ProfilerResultCode::Aborted;
				}

				let mut call_data: FxHashMap<*const MonoMethod, MethodResult> = FxHashMap::default();

				let mut mem_data: FxHashMap<*const MonoClass, MemoryResult> = FxHashMap::default();

				if args.contains(ProfilerArgs::GCEvent) && !gc_out.is_null()
				{
					*gc_out = gc;
				}

				for thread in others.iter() {
					thread.edit_recording(|recording| {
						recording.calls.collect(&mut call_data);
						if !args.contains(ProfilerArgs::FastResume) {
							recording.calls.clear();
						}
						recording.memory.collect(&mut mem_data);
					});
				}
				others.retain(|thread| !thread.retired.load(Ordering::Acquire));

				if let Ok(mut freed) = profiler.freed_methods.lock() {
					call_data.retain(|method, _| !freed.contains(method));
					freed.clear();
				}

				let total_time = now.duration_since(rt.started);

				let mut plugin_data: FxHashMap<*const MonoImage, PluginResults> = FxHashMap::default();
				for (method, method_results) in call_data.iter_mut() {
					let entry = plugin_data.entry(mono_class_get_image(mono_method_get_class(*method))).or_default();
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
				total_time_percentage: percentage_of(result.total_time, *total_time),
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
				assembly_handle: mono_class_get_image(mono_method_get_class(*method)),
				method_handle: *method,
				total_time: result.total_time.as_micros() as u64,
				total_time_percentage: percentage_of(result.total_time, *total_time),
				own_time: result.own_time.as_micros() as u64,
				own_time_percentage: percentage_of(result.own_time, *total_time),
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
		iter: &mut MemoryIter,
		out: &mut MemoryRecord
	) -> bool
	{
		if let Some((class_ptr, result)) = iter.next()
		{
			let class = *class_ptr;
			*out = MemoryRecord
			{
				assembly_handle: mono_class_get_image(class),
				class_token: mono_class_get_type_token(class),
				class_handle: class,
				allocations: result.allocations,
				total_alloc_size: result.total_alloc_size,
				instance_size: mono_class_instance_size(class) as u32
			};
			return true;
		}
		false
	}
}

unsafe fn install_callbacks(profiler: &MonoProfiler, args: ProfilerArgs, instrument_methods: bool) -> bool
{
	let stack = instrument_methods && args.contains(ProfilerArgs::Stack);
	let call_memory = (stack || args.contains(ProfilerArgs::StackWalkAllocations)) && args.contains(ProfilerArgs::CallMemory);
	let advanced_memory = args.contains(ProfilerArgs::AdvancedMemory);
	let gc_events = args.contains(ProfilerArgs::GCEvent);

	if !stack && !call_memory && !advanced_memory && !gc_events
	{
		return false;
	}

	let enter: EnterCB = if args.contains(ProfilerArgs::Timings) {
		method_enter::<true>
	}
	else
	{
		method_enter::<false>
	};
	mono_profiler_set_method_enter_callback(profiler.handle(), stack.then_some(enter));
	mono_profiler_set_method_leave_callback(profiler.handle(), stack.then_some(method_leave as ExitCB));
	mono_profiler_set_method_exception_leave_callback(profiler.handle(), stack.then_some(method_exception_leave as ExceptionalExitCB));
	mono_profiler_set_exception_throw_callback(profiler.handle(), stack.then_some(exception_thrown as ExceptionThrownCB));
	mono_profiler_set_gc_allocation_callback(profiler.handle(), allocation_callback(
		call_memory,
		advanced_memory,
		profiler.gc_alloc_stack_tracking.load(Ordering::Relaxed)));
	mono_profiler_set_gc_event_callback(profiler.handle(), gc_events.then_some(gc_collect_cb as GCEventCB));

	true
}

unsafe fn uninstall_callbacks(profiler: &MonoProfiler)
{
	mono_profiler_set_method_enter_callback(profiler.handle(), None);
	mono_profiler_set_method_leave_callback(profiler.handle(), None);
	mono_profiler_set_method_exception_leave_callback(profiler.handle(), None);
	mono_profiler_set_exception_throw_callback(profiler.handle(), None);
	mono_profiler_set_gc_event_callback(profiler.handle(), None);
	mono_profiler_set_gc_allocation_callback(profiler.handle(), allocation_callback(
		false,
		false,
		profiler.gc_alloc_stack_tracking.load(Ordering::Relaxed)));
}

fn allocation_callback(call: bool, advanced: bool, tracking: bool) -> Option<GCAllocCB>
{
	match (call, advanced, tracking) {
		(false, false, false) => None,
		(false, false, true) => Some(gc_alloc_cb::<false, false, true> as GCAllocCB),
		(false, true, false) => Some(gc_alloc_cb::<false, true, false> as GCAllocCB),
		(false, true, true) => Some(gc_alloc_cb::<false, true, true> as GCAllocCB),
		(true, false, false) => Some(gc_alloc_cb::<true, false, false> as GCAllocCB),
		(true, false, true) => Some(gc_alloc_cb::<true, false, true> as GCAllocCB),
		(true, true, false) => Some(gc_alloc_cb::<true, true, false> as GCAllocCB),
		(true, true, true) => Some(gc_alloc_cb::<true, true, true> as GCAllocCB)
	}
}

pub unsafe extern "C" fn method_free(profiler: &MonoProfiler, method: *const MonoMethod)
{
	#[cfg(debug_assertions)]
	{
		println!("method free");
	}
	if !profiler.is_recording() {return;}
	if let Ok(mut freed) = profiler.freed_methods.lock() {
		freed.insert(method);
	}
}

pub unsafe extern "C" fn can_profile_method(profiler: &MonoProfiler, method: *const MonoMethod) -> CallInstrumentationFlags
{
	if mono_domain_get() != profiler.main_domain {
		return CallInstrumentationFlags::None;
	}
	let sync = ok_or_ret!(profiler.sync.lock(), CallInstrumentationFlags::None);
	let class = mono_method_get_class(method);
	let asm: *const MonoImage = mono_class_get_image(class);
	if mono_class_get_type_token(class) == 0x02000001 {
		// <Module>
		if let Some(name) = read_cstr(mono_method_get_name(method)) {
			if name.to_string_lossy().contains("runtime_invoke")
			{
				return CallInstrumentationFlags::None;
			}
		}
	}
	if sync.profiler_images.contains(&asm)
	{
		INST_FLAGS
	}
	else {
		CallInstrumentationFlags::None
	}
}


#[derive(Default)]
pub struct GcClock
{
	paused: AtomicBool,
	pause_started_at: AtomicU64,
	total_nanos: AtomicU64,
	pauses: AtomicU64
}

impl GcClock
{
	fn start_pause(&self, now: u64)
	{
		self.pause_started_at.store(now, Ordering::Relaxed);
		self.paused.store(true, Ordering::Release);
	}

	fn end_pause(&self, now: u64)
	{
		if self.paused.swap(false, Ordering::Acquire) {
			let started = self.pause_started_at.load(Ordering::Relaxed);
			self.total_nanos.fetch_add(now.saturating_sub(started), Ordering::Relaxed);
			self.pauses.fetch_add(1, Ordering::Relaxed);
		}
	}

	fn take(&self) -> GCRecord
	{
		GCRecord {
			calls: self.pauses.swap(0, Ordering::Relaxed),
			total_time: self.total_nanos.swap(0, Ordering::Relaxed) / 1000
		}
	}
}

pub unsafe extern "C" fn gc_collect_cb(profiler: &MonoProfiler, event: MonoProfilerGCEvent, /* always 0 - true in boehm */ _generation: u32, _serial: MonoBool)
{
	// https://github.com/Unity-Technologies/mono/blob/1c8577ecab30c96474982a3bcfaf4ab2d84fef7a/mono/metadata/boehm-gc.c#L556
	// https://github.com/ivmai/bdwgc/blob/67090a19904291db97ec3a76eb6ae420f3564cea/alloc.c#L825
	match event {
		// GC start
		MONO_GC_EVENT_PRE_STOP_WORLD => {
			if profiler.is_recording() {
				profiler.gc.start_pause(profiler.elapsed_nanos());
			}
		},
		// GC end
		MONO_GC_EVENT_POST_START_WORLD_UNLOCKED => {
			profiler.gc.end_pause(profiler.elapsed_nanos());
		},
		_ => {}
	}
}

pub unsafe extern "C" fn gc_alloc_cb<const CALL: bool, const ADV: bool, const TRACKING: bool>(
	profiler: &MonoProfiler,
	object: *const MonoObject,
) {
	let mut size = None;
	let mut object_size = || *size.get_or_insert_with(|| mono_object_get_size(object) as u64);
	if TRACKING {
		let _ = THREAD_STATE.try_with(|state| {
			if let Ok(mut scopes) = state.allocation_scopes.try_borrow_mut() {
				if scopes.is_tracking() {
					scopes.allocation(object_size());
				}
			}
		});
	}
	if !CALL && !ADV {
		return;
	}
	record(profiler, |recording, _| {
		let alloc = object_size();
		if CALL {
			if profiler.args().contains(ProfilerArgs::StackWalkAllocations) {
				recording.calls.begin_allocation_walk();
				let mut ctx = SimpleCtx {
					calls: &mut recording.calls,
					alloc,
					first: true,
					corlib: profiler.mono_defaults.corlib
				};
				mono_stack_walk::<true, SimpleCtx>(stack_walk_simple, &mut ctx);
			} else {
				recording.calls.allocation(alloc);
			}
		}
		if ADV {
			recording.memory.allocation(mono_object_class(object), alloc);
		}
	});
	struct SimpleCtx<'a> {
		pub calls: &'a mut CallRecorder,
		pub alloc: u64,
		pub first: bool,
		pub corlib: *const MonoImage
	}
	unsafe extern "C" fn stack_walk_simple(
		mptr: *const MonoMethod,
		_native_offset: i32,
		_il_offset: i32,
		_is_managed: MonoBool,
		ctx: &mut SimpleCtx,
	) -> MonoBool {
		if let Some(method) = mptr.as_ref() {
			// exclude mscorlib
			if mono_class_get_image(mono_method_get_class(method)) == ctx.corlib
			{
				return 0;
			}
			ctx.calls.walked_frame(method, ctx.alloc, ctx.first);
			ctx.first = false;
		}
		0
	}
}

#[inline]
pub unsafe extern "C" fn method_enter<const TIMED: bool>(profiler: &MonoProfiler, method: *const MonoMethod, _ctx: *const MonoProfilerCallContext)
{
	record(profiler, |recording, main| {
		recording.calls.enter(method, (TIMED && /* only some on main thread */ main).then(Instant::now));
	});
}

pub unsafe extern "C" fn exception_thrown(profiler: &MonoProfiler, _obj: *const MonoObject)
{
	record(profiler, |recording, _| recording.calls.exception_thrown());
}

pub unsafe extern "C" fn method_leave(profiler: &MonoProfiler, method_ptr: *const MonoMethod, _ctx: *const MonoProfilerCallContext)
{
	exit_method(profiler, method_ptr, false);
}
#[inline]
pub unsafe fn exit_method(profiler: &MonoProfiler, method_ptr: *const MonoMethod, exceptional: bool)
{
	record(profiler, |recording, _| recording.calls.exit(method_ptr, exceptional, Instant::now));
}

pub unsafe extern "C" fn method_exception_leave(profiler: &MonoProfiler, method: *const MonoMethod, _exception: *const MonoObject)
{
	#[cfg(debug_assertions)]
	{
		println!("exception in {}", mono_method_name(method));
	}
	exit_method(profiler, method, true);
}