use std::{fs, mem, ptr, slice, thread};
use std::cell::Cell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ffi::CStr;
use std::mem::transmute;
use std::sync::{Mutex, RwLock};
use std::time::{Duration, Instant};

use bytesize::ByteSize;
use serde_derive::{Deserialize, Serialize};
use tabled::builder::Builder;
use tabled::settings::Style;

use crate::mono::*;
use crate::{log_mono_internal, LogSource, read_cstr, read_cstr_default, run_ffi_safe, Severity};

thread_local! {
	pub static THREAD_DATA: Cell<*mut ThreadData> = const { Cell::new(ptr::null_mut()) }
}

#[repr(u8)]
pub enum ProfilerResultCode
{
	OK = 0,
	MainThreadOnly = 1,
	NotInitialized = 2,
	UnknownError = 3,
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
	pub recording: HashMap<*const MonoMethod, MethodResult>
}

impl Default for ThreadData {
	fn default() -> Self {
		Self {
			main: false,
			stack: Vec::with_capacity(128),
			recording: HashMap::with_capacity(128)
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
	pub handle: MonoProfilerHandle,
	pub sync: RwLock<ProfilerSynced>,
	pub mono_defaults: MonoDefaults,
	pub threads: Mutex<Vec<&'static mut ThreadData>>
}

pub struct ProfilerSynced
{
	pub config: ProfilerConfig,
	pub profiler_images: HashMap<*const MonoImage, String>,
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
	#[serde(rename = "AssembliesToProfile")]
	pub assemblies: Vec<String>
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
	pub fn merge_from(&mut self, input: &MethodResult)
	{
		self.total_time += input.total_time;
		self.own_time += input.own_time;
		self.calls += input.calls;
		self.own_allocations += input.own_allocations;
	}
}

pub struct PluginResults
{
	pub name: String,
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
		mono_profiler_set_method_enter_callback(mp.handle, Some(method_enter));
		mono_profiler_set_method_leave_callback(mp.handle, Some(method_leave));
		//mono_profiler_set_method_tail_call_callback(mp.handle, method_tailcall);
		mono_profiler_set_method_exception_leave_callback(mp.handle, Some(method_exception));
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
pub unsafe extern "system" fn register_profiler_assembly(image_raw: *const MonoImage, strptr: *const u16, len: i32)
{
	let profiler = get_profiler!();
	let mut sync = profiler.sync.write().unwrap();
	if image_raw.is_null() {return;}
	let image = &*image_raw;
	let name: String = if strptr.is_null() || len < 1
	{
		CStr::from_ptr(image.assembly_name).to_str().unwrap().to_string()
	}
	else 
	{
		String::from_utf16(slice::from_raw_parts(strptr, len as usize)).unwrap()
	};
	#[cfg(debug_assertions)]
	{
		println!("registering assembly: {}", name);
	}
	sync.profiler_images.insert(image, name);
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
		sync.profiler_images.insert(image, match name {
			None => get_assembly_name(image),
			Some(x) => x.to_string()
		});
	}
}

pub unsafe fn get_assembly_name(profiler: &MonoImage) -> String
{
	read_cstr_default(profiler.assembly_name, ||{
		format!("__UNKNOWN: {:x}", transmute::<&MonoImage, usize>(profiler))
	})
}

pub static mut PROFILER_RECORDING: bool = false;

#[no_mangle]
pub unsafe extern "system" fn profiler_toggle(gen_advanced: bool, state: &mut bool, basic_out: *mut (), adv_out: *mut (), result_cb: extern "system" fn(*mut (), *const u8, i32)) -> ProfilerResultCode
{
	run_ffi_safe!("profiler_toggle", ProfilerResultCode::UnknownError, {
	let td = get_thread_data();
	if !td.main {return ProfilerResultCode::MainThreadOnly;}
	let profiler = get_profiler!(ProfilerResultCode::NotInitialized);
	match PROFILER_RECORDING {
		false => {log_mono_internal(Severity::Warning, "Profiler recording", LogSource::Profiler, 1);}
		true => {log_mono_internal(Severity::Warning, "Profiler stopped", LogSource::Profiler, 1);}
	}
	let mut sync = profiler.sync.write().unwrap();
	let mut basic_ret: Option<String> = None;
	let mut adv_ret: Option<String> = None;
	match !PROFILER_RECORDING {
		true => {
			sync.runtime = Some(RuntimeData
			{
				started: Instant::now()
			});
			*state = true;
			if sync.config.allocations {
				mono_profiler_set_gc_allocation_callback(profiler.handle, Some(gc_alloc_cb));
			}
			PROFILER_RECORDING = true;
		}
		false => {
			PROFILER_RECORDING = false;
			if sync.config.allocations {
				mono_profiler_set_gc_allocation_callback(profiler.handle, None);
			}
			//println!("Profile complete");
			let now = Instant::now();
			// TODO: do this the right way
			thread::sleep(Duration::from_millis(50));
			
			let rt = mem::take(&mut sync.runtime).unwrap();
			
			let mut data = mem::take(&mut td.recording);
			
			let mut others = profiler.threads.lock().unwrap();
			
			for thread in others.iter_mut() {
				if ptr::eq(*thread, td) {continue;}
				for (method, result) in &mut thread.recording {
					match data.entry(*method) {
						Entry::Occupied(mut x) => x.get_mut().merge_from(result),
						Entry::Vacant(x) => {x.insert(*result);}
					}
				}
				thread.recording.clear();
			}
			
			let total_time = now.duration_since(rt.started).as_millis() as f64;
			{
				// BASIC
				// Assembly, Total Time, Total Time %, Calls, Memory usage
				let mut plugin_data: HashMap<*const MonoImage, PluginResults> = HashMap::with_capacity(sync.profiler_images.len());
				for (method, method_results) in data.iter_mut() {
					let entry = plugin_data.entry((*(**method).class).image).or_insert_with(||{
						PluginResults {
							name: get_asm_from_method(&sync, &**method),
							calls: 0,
							allocations: 0,
							total_time: Duration::ZERO
						}
					});
					entry.calls += method_results.calls;
					entry.allocations += method_results.own_allocations;
					entry.total_time += method_results.own_time;
				}
				// Assembly, Total Time, Total time %, Calls, Memory allocated
				let mut builder: Builder = Builder::with_capacity(data.len(), 5);
				builder.push_record(["Assembly", "Total Time", "Total time %", "Calls", "Memory usage"]);
				for result in plugin_data.into_values() {
					builder.push_record([
						result.name,
						format!("{}", result.total_time.as_millis()),
						format!("{}", ((result.total_time.as_millis() as f64 / total_time) * 100f64).floor()),
						format!("{}", result.calls),
						format!("{}", result.allocations)
					]);
				}


				basic_ret = Some(builder.build().with(Style::empty()).to_string());
			}
			if gen_advanced
			{
				// ADVANCED
				// Method, Total Time, Own Time, Own Time %, Calls
				data.retain(|_k,v|{
					v.total_time.as_millis() > 0 || v.total_allocations > 0
				});
				let mut builder: Builder = Builder::with_capacity(data.len(), 9);
				builder.push_record(["Assembly", "Method", "Total Time", "Total time %", "Own time", "Own time %", "Calls", "Total Alloc", "Own Alloc"]);
				for (method, result) in data.iter_mut() {
					builder.push_record([
						get_asm_from_method(&sync, &**method).to_string(),
						get_method_full_name(&**method, MonoTypeNameFormat::FullName).to_string(),
						format!("{}", result.total_time.as_millis()),
						format!("{}", ((result.total_time.as_millis() as f64 / total_time) * 100f64).floor()),
						format!("{}", result.own_time.as_millis()),
						format!("{}", ((result.own_time.as_millis() as f64 / total_time) * 100f64).floor()),
						format!("{}", result.calls),
						format!("{}", result.total_allocations),
						format!("{}", result.own_allocations)
					]);
				}


				adv_ret = Some(builder.build().with(Style::empty()).to_string());
			}

			data.clear();
			td.recording = data;

			*state = false;
		}
	}
	if let Some(rstr) = basic_ret
	{
		result_cb(basic_out, rstr.as_ptr(), rstr.len() as i32);
	}
	if let Some(rstr) = adv_ret
	{
		result_cb(adv_out, rstr.as_ptr(), rstr.len() as i32);
	}
	ProfilerResultCode::OK
	})
}

pub unsafe fn get_asm_from_method<'a>(profiler: &'a ProfilerSynced, method: &'a MonoMethod) -> String
{
	let image = (*method.class).image;
	match profiler.profiler_images.get(&image) {
		None => get_assembly_name(&*image),
		Some(x) => {
			x.clone()
		}
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
	if sync.profiler_images.contains_key(&asm)
	{
		//debug_log_method(method);
		INST_FLAGS
	}
	else {
		//debug_log_method(method);
		CallInstrumentationFlags::None
	}
}

pub unsafe extern "C" fn method_enter(profiler: &mut MonoProfiler, method: *const MonoMethod, _ctx: *const MonoProfilerCallContext)
{
	enter_method(profiler, method);
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
	if !PROFILER_RECORDING || obj_ptr.is_null() {return;}
	let td = get_thread_data();
	let object= &*obj_ptr;
	let frame: &mut StackFrame = match td.stack.last_mut() {
		None => return,
		Some(x) => x
	};
	let alloc = get_sizeof_object(profiler, object) as u64;
	frame.own_allocations += alloc;
	frame.total_allocations += alloc;
}

/*pub unsafe extern "C" fn method_tailcall(profiler: &mut MonoProfiler, method: *const MonoMethod, target: *const MonoMethod)
{
	let td = get_thread_data();
	println!("tailcall: m-{} t-{}", get_mono_method_name(method), get_mono_method_name(target));
	let frame: &StackFrame = match td.stack.data.last() {
		None => return,
		Some(x) => x
	};
	if ptr::eq(frame.method, method)
	{
		td.stack.data.truncate(td.stack.data.len()-1);
	}
	//enter_method(method);
}*/

#[inline]
pub unsafe fn enter_method(_profiler: &mut MonoProfiler, method: *const MonoMethod)
{
	assert!(!method.is_null());
	let td = get_thread_data();
	td.stack.push(StackFrame::new(method, PROFILER_RECORDING && td.main));
}

#[inline]
pub unsafe fn exit_method(_profiler: &mut MonoProfiler, method_ptr: *const MonoMethod)
{
	let td = get_thread_data();

	let idx: usize;
	let frame: &StackFrame = match td.stack.iter().rposition(|sf| {
		sf.method == method_ptr
	}) {
		None => return,
		Some(x) => {
			idx = x;
			td.stack.get_unchecked(x)
		}
	};

	/*{
		frame = match td.stack.data.last() {
			None => return,
			Some(x) => x
		};
		if !ptr::eq(frame.method, method_ptr)
		{
			for (index, sf) in td.stack.data.iter().enumerate().rev() {
				println!("{}: {}", index, (*sf.method).get_name());
			}
			mono_panic!("stack mismatch: {} (expected), {} (found) {}", name, (*frame.method).get_name(), td.stack.data.len());
		}
	}*/

	if PROFILER_RECORDING {

		let result: &mut MethodResult = td.recording.entry(method_ptr).or_default();

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

		let prev_idx: usize = td.stack.len().saturating_sub(2);

		if let Some(prev) = td.stack.get_mut(prev_idx)
		{
			prev.total_allocations += total_alloc;
			if let Some(prev_time) = &mut prev.time
			{
				prev_time.others += total_time;
			}
		}
	}

	td.stack.truncate(idx);
}

/*pub fn resolve_plugin_name<'a>(profiler: &'a MonoProfiler, mut method: &MonoMethod) -> &'a str
{
	unsafe {
		match profiler.profiler_method_detour_map.get(&(method as *const MonoMethod))
		{
			None => {}
			Some(x) => {
				method = &**x;
			}
		}
		match profiler.profiler_images.get(&(*method.class).image) {
			None => "Unknown",
			Some(x) => x.as_str()
		}
	}
}*/

pub unsafe extern "C" fn method_leave(profiler: &mut MonoProfiler, method: *const MonoMethod, _ctx: *const MonoProfilerCallContext)
{
	exit_method(profiler, method);
}

pub unsafe extern "C" fn method_exception(profiler: &mut MonoProfiler, method: *const MonoMethod, _exception: *const MonoObject)
{
	exit_method(profiler, method);
}