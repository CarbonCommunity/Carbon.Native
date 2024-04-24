use std::{fs, mem, ptr, slice, thread};
use std::cell::Cell;
use std::collections::hash_map::{Entry, Iter};
use std::collections::{HashMap, HashSet};
use std::ffi::CStr;
use std::mem::transmute;
use std::sync::{Mutex, RwLock};
use std::time::{Duration, Instant};
use bitflags::bitflags;

use serde_derive::{Deserialize, Serialize};

use crate::{log_mono_internal, LogSource, read_cstr, read_cstr_default, run_ffi_safe, Severity};
use crate::mono::*;

thread_local! {
	pub static THREAD_DATA: Cell<*mut ThreadData> = const { Cell::new(ptr::null_mut()) }
}

#[repr(u8)]
pub enum ProfilerResultCode
{
	OK = 0,
	Aborted = 1,
	MainThreadOnly = 2,
	NotInitialized = 3,
	UnknownError = 4,
}

pub struct StackFrame
{
	pub method: *const MonoMethod,
	pub own_allocations: u64,
	pub total_allocations: u64,
	pub time: Option<StackTiming>,
}

macro_rules! get_profiler {
    () => {
		match &mut PROFILER {
			None => return,
			Some(x) => x
		}
	};
	($default:expr) => {
		match &mut PROFILER {
			None => {return $default;},
			Some(x) => x
		}
	};
}

pub struct StackTiming
{
	pub time: Instant,
	pub others: Duration
}

impl StackFrame
{
	pub fn new(method: *const MonoMethod, timing: bool) -> Self
	{
		StackFrame
		{
			own_allocations: 0,
			total_allocations: 0,
			time: if timing
			{
				Some(StackTiming
				{
					time: Instant::now(),
					others: Duration::ZERO
				})
			}
			else {
				None
			},
			method
		}
	}
}

pub struct ThreadData
{
	pub main: bool,
	pub stack: Vec<StackFrame>,
	pub call_recording: HashMap<*const MonoMethod, MethodResult>,
	//pub memory_recording: HashMap<*const MonoClass, MemoryResult>
}

impl Default for ThreadData {
	fn default() -> Self {
		Self {
			main: false,
			stack: Vec::with_capacity(128),
			call_recording: HashMap::with_capacity(128),
			//memory_recording: HashMap::with_capacity(64)
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
	pub args: ProfilerArgs,
	pub handle: MonoProfilerHandle,
	pub sync: RwLock<ProfilerSynced>,
	pub mono_defaults: MonoDefaults,
	pub threads: Mutex<Vec<&'static mut ThreadData>>
}

pub struct ProfilerSynced
{
	pub config: ProfilerConfig,
	pub profiler_images: HashSet<*const MonoImage>,
	pub profiler_method_image_map: HashMap<*const MonoMethod, *const MonoImage>,
	pub profiler_method_detour_map: HashMap<*const MonoMethod, *const MonoMethod>,
	pub runtime: Option<RuntimeData>,
}

#[derive(Serialize, Deserialize, Default)]
pub struct ProfilerConfig
{
	#[serde(rename = "Enabled")]
	pub enabled: bool,
	#[serde(rename = "Allocations")]
	pub allocations: bool,
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
	}
}

#[derive(Default, Copy, Clone)]
pub struct MemoryResult
{
	pub allocations: u64,
	pub total_alloc_size: u64
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

pub struct PluginResults
{
	pub calls: u64,
	pub allocations: u64,
	pub total_time: Duration,
}

pub fn load_config(path: &str) -> anyhow::Result<ProfilerConfig>
{
	anyhow::Ok(serde_json::from_slice(fs::read(path)?.as_slice())?)
}

pub fn get_default_config(path: &str) -> ProfilerConfig
{
	let cfg = ProfilerConfig::default();
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

		get_thread_data().main = true;

		let mp = PROFILER.as_mut().unwrap();

		mp.handle = mono_profiler_create(mp);

		if mp.sync.read().unwrap().config.allocations { 
			mono_profiler_enable_allocations();
		}
		mono_profiler_set_call_instrumentation_filter_callback(mp.handle, Some(can_profile_method));
		mono_profiler_set_image_loaded_callback(mp.handle, Some(image_loaded));
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
	let profiler = get_profiler!();
	let mut sync = profiler.sync.write().unwrap();
	if image_raw.is_null() {return;}
	let image = &*image_raw;
	#[cfg(debug_assertions)]
	{
		println!("registering assembly from managed: {}", get_assembly_name(image));
	}
	sync.profiler_images.insert(image);
}

pub unsafe extern "C" fn image_loaded(profiler: &mut MonoProfiler, image: &MonoImage)
{
	let mut sync = profiler.sync.write().unwrap();
	
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

#[repr(C)]
pub struct BasicRecord
{
	pub assembly_handle: *const MonoImage,
	pub total_time: u64,
	pub total_time_percentage: f64,
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
	pub method_name: *const MonoString,
	pub total_time: u64,
	pub total_time_percentage: f64,
	pub own_time: u64,
	pub own_time_percentage: f64,
	pub calls: u64,
	pub total_alloc: u64,
	pub own_alloc: u64,
}

impl Default for AdvancedRecord
{
	fn default() -> Self {
		Self
		{
			assembly_handle: ptr::null(),
			method_handle: ptr::null(),
			method_name: ptr::null_mut(),
			total_time: 0,
			total_time_percentage: 0f64,
			own_time: 0,
			own_time_percentage: 0f64,
			calls: 0,
			total_alloc: 0,
			own_alloc: 0
		}
	}
}

#[repr(C)]
pub struct MemoryRecord
{
	pub klass_handle: *const MonoClass,
	pub klass_name: *const MonoString,
	pub allocations: u64,
	pub total_alloc_size: u64
}

impl Default for MemoryRecord
{
	fn default() -> Self {
		Self
		{
			klass_handle: ptr::null(),
			klass_name: ptr::null(),
			allocations: 0,
			total_alloc_size: 0
		}
	}
}

pub type ManagedStringMarshalFunc = extern "system" fn(&mut *const MonoString, *const u8, i32);
pub type ManagedArrMarshalFunc<T> = extern "system" fn(&mut *const TypedMonoArray<T>, *const T, u64);
pub type BasicIter<'a> = (Iter<'a, *const MonoImage, PluginResults>, f64);
pub type AdvIter<'a> = (Iter<'a, *const MonoMethod, MethodResult>, ManagedStringMarshalFunc, f64);
pub type MemoryIter<'a> = (Iter<'a, *const MonoClass, MemoryResult>);

#[no_mangle]
pub unsafe extern "system" fn get_image_name(managed: &mut *const MonoString, image: *const MonoImage, marshal: ManagedStringMarshalFunc)
{
	if let Some(cs) = read_cstr((*image).assembly_name)
	{
		marshal(managed, cs.as_ptr(), cs.len() as i32);
	}
}

bitflags! {
	#[repr(C)]
	#[derive(PartialEq)]
	pub struct ProfilerArgs: u8
	{
		const None = 0;
		const Abort = 1 << 0;
		const Advanced = 1 << 1;
		const Memory = 1 << 2;
		const AdvancedMemory = 1 << 3; // TODO: add advanced memory profiling
		const Timings = 1 << 4;
	}
}

#[no_mangle]
pub unsafe extern "system" fn profiler_toggle(
	args: ProfilerArgs,
	state: &mut bool,
	basic_out: *mut (),
	adv_out: *mut (),
	marshal_string: ManagedStringMarshalFunc,
	basic_iter_cb: extern "system" fn(*mut (), u64, &mut BasicIter, unsafe extern "system" fn(&mut BasicIter, &mut BasicRecord) -> bool),
	advanced_iter_cb: extern "system" fn(*mut (), u64, &mut AdvIter, unsafe extern "system" fn(&mut AdvIter, &mut AdvancedRecord) -> bool),
	//memory_iter_cb: extern "system" fn(*mut (), u64, &mut MemoryIter, unsafe extern "system" fn(&mut MemoryIter, &mut MemoryResult) -> bool),
) -> ProfilerResultCode
{
	return run_ffi_safe!("profiler_toggle", ProfilerResultCode::UnknownError, {
		let td = get_thread_data();
		if !td.main {return ProfilerResultCode::MainThreadOnly;}
		let profiler = get_profiler!(ProfilerResultCode::NotInitialized);
		let mut others = profiler.threads.lock().unwrap();
		let mut sync = profiler.sync.write().unwrap();
		match !profiler.profiler_recording {
			true => {
				if args.contains(ProfilerArgs::Abort)
				{
					return ProfilerResultCode::Aborted;
				}
				sync.runtime = Some(RuntimeData
				{
					started: Instant::now()
				});
				*state = true;
				if sync.config.allocations {
					mono_profiler_set_gc_allocation_callback(profiler.handle, Some(gc_alloc_cb));
				}
				mono_profiler_set_method_enter_callback(profiler.handle, Some(enter_method));
				mono_profiler_set_method_leave_callback(profiler.handle, Some(exit_method));
				mono_profiler_set_method_exception_leave_callback(profiler.handle, Some(method_exception));
				profiler.args = args;
				profiler.profiler_recording = true;
				drop(sync);
			}
			false => {
				profiler.profiler_recording = false;
				profiler.args = ProfilerArgs::None;
				mono_profiler_set_method_enter_callback(profiler.handle, None);
				mono_profiler_set_method_leave_callback(profiler.handle, None);
				mono_profiler_set_method_exception_leave_callback(profiler.handle, None);
				if sync.config.allocations {
					mono_profiler_set_gc_allocation_callback(profiler.handle, None);
				}
				//println!("Profile complete");
				let now = Instant::now();
				// TODO: do this the right way
				thread::sleep(Duration::from_millis(50));

				let rt = mem::take(&mut sync.runtime).unwrap();

				let mut data = mem::take(&mut td.call_recording);
				
				if args.contains(ProfilerArgs::Abort)
				{
					for thread in others.iter_mut() {
						thread.call_recording.clear();
						thread.stack.clear();
					}
					data.clear();
					td.call_recording = data;
					return ProfilerResultCode::Aborted;
				}

				for thread in others.iter_mut() {
					thread.stack.clear();
					if ptr::eq(*thread, td) {continue;}
					for (method, result) in &mut thread.call_recording {
						match data.entry(*method) {
							Entry::Occupied(mut x) => x.get_mut().merge_from(result),
							Entry::Vacant(x) => {x.insert(*result);}
						}
					}
					thread.call_recording.clear();
				}

				let mut basic_ret: BasicIter;

				let total_time = now.duration_since(rt.started).as_millis() as f64;
				
				let mut plugin_data: HashMap<*const MonoImage, PluginResults> = HashMap::with_capacity(sync.profiler_images.len());
				for (method, method_results) in data.iter_mut() {
					let entry = plugin_data.entry((*(**method).class).image).or_insert_with(||{
						PluginResults {
							calls: 0,
							allocations: 0,
							total_time: Duration::ZERO
						}
					});
					entry.calls += method_results.calls;
					entry.allocations += method_results.own_allocations;
					entry.total_time += method_results.own_time;
				}
				basic_ret = (plugin_data.iter(), total_time);

				*state = false;

				drop(sync);
				
				basic_iter_cb(basic_out, plugin_data.len() as u64, &mut basic_ret, iter_basic_fn);
				
				if args.contains(ProfilerArgs::Advanced)
				{
					data.retain(|_k,v|{
						v.total_time.as_millis() > 0 || v.total_allocations > 0
					});
					advanced_iter_cb(adv_out, data.len() as u64, &mut (data.iter(), marshal_string, total_time), iter_advanced_fn);
				}
				
				data.clear();
				td.call_recording = data;
			}
		}
		match profiler.profiler_recording {
			false => {log_mono_internal(Severity::Warning, "Finished recording", LogSource::Profiler, 1);}
			true => {log_mono_internal(Severity::Warning, "Recording started..", LogSource::Profiler, 1);}
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
				total_time: result.total_time.as_millis() as u64,
				total_time_percentage: (result.total_time.as_millis() as f64 / *total_time) * 100f64,
				calls: result.calls,
				alloc: result.allocations
			};
			return true;
		}
		false
	}
	unsafe extern "system" fn iter_advanced_fn(
		(iter, marshal_string, total_time): &mut AdvIter,
		out: &mut AdvancedRecord
	) -> bool
	{
		if let Some((method, result)) = iter.next()
		{
			let fullname = get_method_full_name(&**method, MonoTypeNameFormat::FullName);
			*out = AdvancedRecord
			{
				assembly_handle: (*(*(*method)).class).image,
				method_handle: *method,
				total_time: result.total_time.as_millis() as u64,
				total_time_percentage: (result.total_time.as_millis() as f64 / *total_time) * 100f64,
				own_time: result.own_time.as_millis() as u64,
				own_time_percentage: (result.own_time.as_millis() as f64 / *total_time) * 100f64,
				calls: result.calls,
				total_alloc: result.total_allocations,
				own_alloc: result.own_allocations, 
				..Default::default()
			};
			marshal_string(&mut out.method_name, fullname.as_ptr(), fullname.len() as i32);
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
pub unsafe extern "C" fn method_free(_profiler: &mut MonoProfiler, method: &MonoMethod)
{
	debug_log_method(method);
	println!("method free");
}

pub unsafe extern "C" fn can_profile_method(profiler: &mut MonoProfiler, mut method: &MonoMethod) -> CallInstrumentationFlags
{
	let sync = profiler.sync.read().unwrap();
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

pub unsafe extern "C" fn gc_alloc_cb(profiler: &mut MonoProfiler, obj_ptr: *const MonoObject)
{
	if !profiler.profiler_recording || !profiler.args.contains(ProfilerArgs::Memory) || obj_ptr.is_null() {return;}
	let td = get_thread_data();
	let object= &*obj_ptr;
	let frame: &mut StackFrame = match td.stack.last_mut() {
		None => return,
		Some(x) => x
	};
	let alloc = get_sizeof_object(profiler, object) as u64;
	frame.own_allocations += alloc;
	frame.total_allocations += alloc;
	
	//if profiler.args.contains(ProfilerArgs::AdvancedMemory)
	{
		// TODO: advanced memory
	}
}

#[inline]
pub unsafe extern "C" fn enter_method(profiler: &mut MonoProfiler, method: &MonoMethod, _ctx: *const MonoProfilerCallContext)
{
	if !profiler.profiler_recording { return; }
	let td = get_thread_data();
	td.stack.push(StackFrame::new(method, td.main && profiler.args.contains(ProfilerArgs::Timings)));
}

pub unsafe extern "C" fn exit_method(profiler: &mut MonoProfiler, method_ptr: *const MonoMethod, _ctx: *const MonoProfilerCallContext)
{
	if !profiler.profiler_recording { return; }

	let td = get_thread_data();

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

	let result: &mut MethodResult = td.call_recording.entry(method_ptr).or_default();

	result.calls += 1;

	result.own_allocations += frame.own_allocations;

	let total_alloc = frame.total_allocations;

	result.total_allocations += total_alloc;

	let mut total_time: Duration = Duration::ZERO;

	if let Some(frame_time) = &frame.time
	{
		let now = Instant::now();
		total_time = now.duration_since(frame_time.time);
		result.total_time += total_time;
		result.own_time += total_time - frame_time.others;
	}

	if let Some(prev_idx) = idx.checked_sub(1)
	{
		let prev = td.stack.get_unchecked_mut(prev_idx);
		prev.total_allocations += total_alloc;
		if let Some(prev_time) = &mut prev.time
		{
			prev_time.others += total_time;
		}
	}

	td.stack.truncate(idx);
}


pub unsafe extern "C" fn method_exception(profiler: &mut MonoProfiler, method: *const MonoMethod, _exception: *const MonoObject)
{
	exit_method(profiler, method, ptr::null());
}