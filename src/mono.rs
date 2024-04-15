use std::ffi::{c_char, CStr};
use std::mem::transmute;
use std::slice;

use bitflags::bitflags;

use crate::profiler::MonoProfiler;

#[cfg_attr(target_os = "windows", link(name = "mono-2.0-bdwgc", kind = "raw-dylib"))]
#[cfg_attr(target_os = "linux", link(name = "monobdwgc-2.0", kind = "dylib"))]
extern "C" {
	fn mono_free(ptr: *mut ());
	pub fn mono_type_get_name_full(mtype: *const MonoType, format: MonoTypeNameFormat) -> *mut c_char;
	pub fn mono_get_string_class() -> *const MonoClass;
	pub fn mono_profiler_enable_allocations() -> bool;
	pub fn mono_profiler_set_gc_allocation_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&mut MonoProfiler, *const MonoObject)>);
	pub fn mono_profiler_set_image_loaded_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&mut MonoProfiler, &MonoImage)>);
	pub fn mono_profiler_set_method_enter_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&mut MonoProfiler, *const MonoMethod, *const MonoProfilerCallContext)>);
	pub fn mono_profiler_set_method_tail_call_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&mut MonoProfiler, *const MonoMethod, *const MonoMethod)>);
	pub fn mono_profiler_set_method_leave_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&mut MonoProfiler, *const MonoMethod, *const MonoProfilerCallContext)>);
	pub fn mono_profiler_set_method_exception_leave_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&mut MonoProfiler, *const MonoMethod, *const MonoObject)>);
	pub fn mono_profiler_set_method_free_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&mut MonoProfiler, &MonoMethod)>);
	pub fn mono_profiler_set_call_instrumentation_filter_callback(handle: MonoProfilerHandle, cb: Option<unsafe extern "C" fn(&mut MonoProfiler, &MonoMethod) -> CallInstrumentationFlags>);
	pub fn mono_profiler_create(info: *mut MonoProfiler) -> MonoProfilerHandle;
}

pub unsafe fn mono_free_g<T>(ptr: *mut T)
{
	mono_free(transmute(ptr));
}

pub struct MonoDefaults
{
	pub string_class: *const MonoClass
}

impl MonoDefaults
{
	pub fn new() -> MonoDefaults
	{
		unsafe {
			MonoDefaults {
				string_class: mono_get_string_class()
			}
		}
	}
}

unsafe impl Sync for MonoProfiler
{
	
}

pub static mut PROFILER: Option<MonoProfiler> = None;


bitflags! {
	pub struct CallInstrumentationFlags : u8
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

#[repr(C)]
pub struct MonoMethod
{
	pub flags: u16,
	pub iflags: u16,
	pub token: u32,
	pub class: *const MonoClass,
	pub signature: *const (),
	pub name: *const c_char,
	/*#ifdef IL2CPP_ON_MONO
	void* method_pointer;
	void* invoke_pointer;
	#endif
	/* this is used by the inlining algorithm */
	unsigned int inline_info:1;
	unsigned int inline_failure:1;
	unsigned int wrapper_type:5;
	unsigned int string_ctor:1;
	unsigned int save_lmf:1;
	unsigned int dynamic:1; /* created & destroyed during runtime */
	unsigned int sre_method:1; /* created at runtime using Reflection.Emit */
	unsigned int is_generic:1; /* whenever this is a generic method definition */
	unsigned int is_inflated:1; /* whether we're a MonoMethodInflated */
	unsigned int skip_visibility:1; /* whenever to skip JIT visibility checks */
	unsigned int _unused : 2; /* unused */
	signed int slot : 16;*/
}

const NULL_PTR: &str = "__NULLPTR";

impl MonoMethod
{
	pub fn get_name(&self) -> &str
	{
		if self.name.is_null()
		{
			return NULL_PTR;
		}
		unsafe {
			CStr::from_ptr(self.name).to_str().unwrap()
		}
	}
}

pub fn get_mono_method_name(method: *const MonoMethod) -> &'static str
{
	unsafe {
		if method.is_null()
		{
			return NULL_PTR;
		}
		(*method).get_name()
	}
}

pub fn get_method_full_name(method: &MonoMethod, format: MonoTypeNameFormat) -> String
{
	unsafe {
		let nameptr = mono_type_get_name_full(&(*method.class)._byval_arg, format);
		
		let ret = format!("{}::{}", CStr::from_ptr(nameptr).to_str().unwrap(), CStr::from_ptr(method.name).to_str().unwrap());
		mono_free_g(nameptr);
		ret
	}
}

#[repr(u8)]
pub enum MonoTypeKind
{
	/// non-generic type
	ClassDef = 1,
	/// generic type definition
	ClassGtd,
	/// generic instantiation
	ClassGinst,
	/// generic parameter
	ClassGparam,
	/// vector or array, bounded or not
	ClassArray,
	/// pointer or function pointer
	ClassPointer,
	/// not a real class kind - used for sgen nursery filler arrays
	ClassGcFiller = 0xAC
}

#[repr(C)]
pub struct MonoClass {
	/* element class for arrays and enum basetype for enums */
	pub element_class: *const MonoClass,
	/* used for subtype checks */
	pub cast_class: *const MonoClass,

	/* for fast subtype checks */
	pub supertypes: *const *const MonoClass, 
	pub idepth: guint16,

	/* array dimension */
	pub rank: guint8,

	/* One of the values from MonoTypeKind */
	pub class_kind: MonoTypeKind,

	pub instance_size: int, /* object instance size */

	// C bitfield bullshit
	
	/*pub inited: guint,

	/* A class contains static and non static data. Static data can be
	 * of the same type as the class itselfs, but it does not influence
	 * the instance size of the class. To avoid cyclic calls to 
	 * mono_class_init_internal (from mono_class_instance_size ()) we first
	 * initialise all non static fields. After that we set size_inited 
	 * to 1, because we know the instance size now. After that we 
	 * initialise all static fields.
	 */

	/* ALL BITFIELDS SHOULD BE WRITTEN WHILE HOLDING THE LOADER LOCK */
	pub size_inited     : guint,
	pub valuetype       : guint, /* derives from System.ValueType */
	pub enumtype        : guint, /* derives from System.Enum */
	pub blittable       : guint, /* class is blittable */
	pub unicode         : guint, /* class uses unicode char when marshalled */
	pub wastypebuilder  : guint, /* class was created at runtime from a TypeBuilder */
	pub is_array_special_interface : guint, /* gtd or ginst of once of the magic interfaces that arrays implement */
	pub is_byreflike    : guint, /* class is a valuetype and has System.Runtime.CompilerServices.IsByRefLikeAttribute */*/
	
	#[cfg(target_os = "windows")] // aaaaaaaaaaaaaaaaaa
	_pd1: [u8; 4],

	/* next byte */
	pub min_align: guint8,

	/* next byte */
	/*pub packing_size    : guint,
	pub ghcimpl         : guint, /* class has its own GetHashCode impl */
	pub has_finalize    : guint, /* class has its own Finalize impl */
	//#ifndef DISABLE_REMOTING
	pub marshalbyref    : guint, /* class is a MarshalByRefObject */
	pub contextbound    : guint, /* class is a ContextBoundObject */
	//#endif*/
	/* next byte */
	/*pub delegate        : guint, /* class is a Delegate */
	pub gc_descr_inited : guint, /* gc_descr is initialized */
	pub has_cctor       : guint, /* class has a cctor */
	pub has_references  : guint, /* it has GC-tracked references in the instance */
	pub has_static_refs : guint, /* it has static fields that are GC-tracked */
	pub no_special_static_fields : guint, /* has no thread/context static fields */
	/* directly or indirectly derives from ComImport attributed class.
	 * this means we need to create a proxy for instances of this class
	 * for COM Interop. set this flag on loading so all we need is a quick check
	 * during object creation rather than having to traverse supertypes
	 */
	pub is_com_object : guint,
	pub nested_classes_inited : guint, /* Whenever nested_class is initialized */ */

	/* next byte*/
	/*pub interfaces_inited : guint, /* interfaces is initialized */
	pub simd_type : guint, /* class is a simd intrinsic type */
	pub has_finalize_inited    : guint, /* has_finalize is initialized */
	pub fields_inited : guint, /* setup_fields () has finished */
	pub has_failure : guint, /* See mono_class_get_exception_data () for a MonoErrorBoxed with the details */
	pub has_weak_fields : guint, /* class has weak reference fields */
	pub has_dim_conflicts : guint, /* Class has conflicting default interface methods */ */
	#[cfg(target_os = "windows")]
	_pd2: [u8; 4],
	

	pub parent: *const MonoClass,
	pub nested_in: *const MonoClass,

	pub image: *const MonoImage,
	pub name: *const c_char,
	pub name_space: *const c_char,

	pub type_token: guint32,
	pub vtable_size: int, /* number of slots */

	pub interface_count: guint16,
	pub interface_id: guint32,        /* unique inderface id (for interfaces) */
	pub max_interface_id: guint32,

	pub interface_offsets_count: guint16,
	pub interfaces_packed: *const *const MonoClass,
	pub interface_offsets_packed: *const guint16,
	pub interface_bitmap: *const guint8,

	pub interfaces: *const *const MonoClass,

	pub sizes: MonoClassSizes,

	/*
	 * Field information: Type and location from object base
	 */
	pub fields: *const MonoClassField,

	pub methods: *const *const MonoMethod,

	/* used as the type of the this argument and when passing the arg by value */
	pub this_arg: MonoType,
	pub _byval_arg:  MonoType,

	/*MonoGCDescriptor gc_descr,

	MonoClassRuntimeInfo *runtime_info,

	/* Generic vtable. Initialized by a call to mono_class_setup_vtable () */
	vtable: *const *const MonoMethod,
	/* Infrequently used items. See class-accessors.c: InfrequentDataKind for what goes into here. */
	MonoPropertyBag infrequent_data,

	unity_user_data: *const (),*/
}

#[repr(C)]
pub union MonoClassSizes {
	pub class_size: int, /* size of area for static fields */
	pub element_size: int, /* for array types */
	pub generic_param_token: int, /* for generic param types, both var and mvar */
}

#[repr(C)]
pub struct MonoObject
{
	pub vtable: *const MonoVTable,
	pub sync: *const MonoThreadsSync,
}

impl MonoObject {
	pub fn as_string_checked(&self) -> Option<&MonoString>
	{
		unsafe {
			match &PROFILER {
				None => None,
				Some(x) => {
					if (*self.vtable).klass == x.mono_defaults.string_class
					{
						Some(transmute(self))
					}
					else { 
						None
					}
				}
			}
		}
	}
}

#[repr(C)]
pub struct MonoString
{
	pub base: MonoObject,
	pub length: i32,
	pub str: u16
}

type MonoArraySize = u32;
type MonoArrayLowerBound = i32;

#[repr(C)]
pub struct MonoArrayBounds
{
	pub length: MonoArraySize,
	pub lower_bound: MonoArrayLowerBound
}

#[repr(C)]
pub struct MonoArray
{
	pub base: MonoObject,
	pub bounds: *const MonoArrayBounds,
	pub length: MonoArraySize,
	pub data: *mut u8
}

#[repr(C)]
pub struct TypedMonoArray<T>
{
	pub base: MonoObject,
	pub bounds: *const MonoArrayBounds,
	pub length: MonoArraySize,
	pub data: *mut T
}

impl<T> TypedMonoArray<T>
{
	pub fn get(&self) -> Option<&[T]>
	{
		unsafe {
			if self.bounds.is_null() {
				return Some(slice::from_raw_parts(self.data, self.length as usize));
			}

			return None;
		}
	}
	pub fn get_mut(&mut self) -> Option<&mut [T]>
	{
		unsafe {
			if self.bounds.is_null() {
				return Some(slice::from_raw_parts_mut(self.data, self.length as usize));
			}

			return None;
		}
	}
}

pub type MonoProfilerHandle = *const ();

pub type guint8 = u8;
pub type guint16 = u16;
pub type guint32 = u32;
pub type guint64 = u64;
pub type guint = u32;
pub type gint = i32;
pub type int = i32;

pub type MonoThreadsSync = ();
#[repr(C)]
pub struct MonoVTable
{ 
	pub klass: *const MonoClass,
	/*
	* According to comments in gc_gcj.h, this should be the second word in
	* the vtable.
	*/
	gc_descr: *const (),
	domain: *const MonoDomain,  /* each object/vtable belongs to exactly one domain */
	mtype: *const (), /* System.Type type for klass */
	interface_bitmap: *const guint8,
	max_interface_id: guint32,
	rank: guint8,
	/* Keep this a guint8, the jit depends on it */
	initialized: guint8, /* cctor has been run */
	/* Keep this a guint8, the jit depends on it */
	flags: guint8, /* MonoVTableFlags */
	remote          : guint, /* class is remotely activated */
	init_failed     : guint, /* cctor execution failed */
	has_static_fields : guint, /* pointer to the data stored at the end of the vtable array */
	gc_bits         : guint, /* Those bits are reserved for the usaged of the GC */

	imt_collisions_bitmap: guint32,
	runtime_generic_context: *const MonoRuntimeGenericContext,
	/* interp virtual method table */
	interp_vtable: *const (),
	/* do not add any fields after vtable, the structure is dynamically extended */
	/* vtable contains function pointers to methods or their trampolines, at the
	 end there may be a slot containing the pointer to the static fields */
	vtable: *const (),
}
pub type MonoImageStorage = ();
#[repr(C)]
pub struct MonoImage
{
	/*
	 * This count is incremented during these situations:
	 *   - An assembly references this MonoImage through its 'image' field
	 *   - This MonoImage is present in the 'files' field of an image
	 *   - This MonoImage is present in the 'modules' field of an image
	 *   - A thread is holding a temporary reference to this MonoImage between
	 *     calls to mono_image_open and mono_image_close ()
	 */
	pub ref_count: int,

	pub storage: *const MonoImageStorage,

	/* Aliases storage->raw_data when storage is non-NULL. Otherwise NULL. */
	pub raw_data: *const c_char,
	pub raw_data_len: guint32,

	/*
	
	/* Whenever this is a dynamically emitted module */
	guint8 dynamic : 1;

	/* Whenever this is a reflection only image */
	guint8 ref_only : 1;

	/* Whenever this image contains uncompressed metadata */
	guint8 uncompressed_metadata : 1;

	/* Whenever this image contains metadata only without PE data */
	guint8 metadata_only : 1;

	/*  Whether this image belongs to load-from context */
	guint8 load_from_context: 1;

	guint8 checked_module_cctor : 1;
	guint8 has_module_cctor : 1;

	guint8 idx_string_wide : 1;
	guint8 idx_guid_wide : 1;
	guint8 idx_blob_wide : 1;

	/* Whenever this image is considered as platform code for the CoreCLR security model */
	guint8 core_clr_platform_code : 1;

	/* Whether a #JTD stream was present. Indicates that this image was a minimal delta and its heaps only include the new heap entries */
	guint8 minimal_delta : 1;
	
	 */
	#[cfg(target_os = "windows")]
	_pd1: [u8; 2],

	/* The path to the file for this image or an arbitrary name for images loaded from data. */
	pub name: *mut c_char,

	/* The path to the file for this image or NULL */
	pub filename: *mut c_char,

	/* The assembly name reported in the file for this image (expected to be NULL for a netmodule) */
	pub assembly_name: *const c_char,

	/* The module name reported in the file for this image (could be NULL for a malformed file) */
	pub module_name: *const c_char,
	/*
	guint32 time_date_stamp;
	 

	char *version;
	gint16 md_version_major, md_version_minor;
	char *guid;
	MonoCLIImageInfo    *image_info;
	MonoMemPool         *mempool; /*protected by the image lock*/

	char                *raw_metadata;

	MonoStreamHeader     heap_strings;
	MonoStreamHeader     heap_us;
	MonoStreamHeader     heap_blob;
	MonoStreamHeader     heap_guid;
	MonoStreamHeader     heap_tables;
	MonoStreamHeader     heap_pdb;

	const char          *tables_base;

	/* For PPDB files */
	guint64 referenced_tables;
	int *referenced_table_rows;

	/**/
	MonoTableInfo        tables [MONO_TABLE_NUM];

	/*
	 * references is initialized only by using the mono_assembly_open
	 * function, and not by using the lowlevel mono_image_open.
	 *
	 * Protected by the image lock.
	 *
	 * It is NULL terminated.
	 */
	MonoAssembly **references;
	int nreferences;

	/* Code files in the assembly. The main assembly has a "file" table and also a "module"
	 * table, where the module table is a subset of the file table. We track both lists,
	 * and because we can lazy-load them at different times we reference-increment both.
	 */
	/* No netmodules in netcore, but for System.Reflection.Emit support we still use modules */
	MonoImage **modules;
	guint32 module_count;
	gboolean *modules_loaded;

	MonoImage **files;
	guint32 file_count;

	MonoAotModule *aot_module;

	guint8 aotid[16];

	/*
	 * The Assembly this image was loaded from.
	 */
	MonoAssembly *assembly;

	#ifdef ENABLE_NETCORE
	/*
	 * The AssemblyLoadContext that this image was loaded into.
	 */
	MonoAssemblyLoadContext *alc;
	#endif

	/*
	 * Indexed by method tokens and typedef tokens.
	 */
	GHashTable *method_cache; /*protected by the image lock*/
	MonoInternalHashTable class_cache;

	/* Indexed by memberref + methodspec tokens */
	GHashTable *methodref_cache; /*protected by the image lock*/

	/*
	 * Indexed by fielddef and memberref tokens
	 */
	MonoConcurrentHashTable *field_cache; /*protected by the image lock*/

	/* indexed by typespec tokens. */
	MonoConcurrentHashTable *typespec_cache; /* protected by the image lock */
	/* indexed by token */
	GHashTable *memberref_signatures;

	/* Indexed by blob heap indexes */
	GHashTable *method_signatures;

	/*
	 * Indexes namespaces to hash tables that map class name to typedef token.
	 */
	GHashTable *name_cache;  /*protected by the image lock*/

	/*
	 * Indexed by MonoClass
	 */
	GHashTable *array_cache;
	GHashTable *ptr_cache;

	GHashTable *szarray_cache;
	/* This has a separate lock to improve scalability */
	mono_mutex_t szarray_cache_lock;

	/*
	 * indexed by SignaturePointerPair
	 */
	GHashTable *native_func_wrapper_cache;

	/*
	 * indexed by MonoMethod pointers 
	 */
	GHashTable *wrapper_param_names;
	GHashTable *array_accessor_cache;

	/*
	 * indexed by MonoClass pointers
	 */
	GHashTable *ldfld_wrapper_cache;
	GHashTable *ldflda_wrapper_cache;
	GHashTable *stfld_wrapper_cache;
	GHashTable *isinst_cache;

	GHashTable *icall_wrapper_cache;
	GHashTable *castclass_cache;
	GHashTable *proxy_isinst_cache;
	GHashTable *rgctx_template_hash; /* LOCKING: templates lock */

	/* Contains rarely used fields of runtime structures belonging to this image */
	MonoPropertyHash *property_hash;

	void *reflection_info;

	/*
	 * user_info is a public field and is not touched by the
	 * metadata engine
	 */
	void *user_info;

	#ifndef DISABLE_DLLMAP
	/* dll map entries */
	MonoDllMap *dll_map;
	#endif

	/* interfaces IDs from this image */
	/* protected by the classes lock */
	MonoBitSet *interface_bitset;

	/* when the image is being closed, this is abused as a list of
	   malloc'ed regions to be freed. */
	GSList *reflection_info_unregister_classes;

	/* List of dependent image sets containing this image */
	/* Protected by image_sets_lock */
	GSList *image_sets;

	/* Caches for wrappers that DO NOT reference generic */
	/* arguments */
	MonoWrapperCaches wrapper_caches;

	/* Pre-allocated anon generic params for the first N generic
	 * parameters, for a small N */
	MonoGenericParam *var_gparam_cache_fast;
	MonoGenericParam *mvar_gparam_cache_fast;
	/* Anon generic parameters past N, if needed */
	MonoConcurrentHashTable *var_gparam_cache;
	MonoConcurrentHashTable *mvar_gparam_cache;

	#ifndef ENABLE_NETCORE
	/* Maps malloc-ed char* pinvoke scope -> MonoDl* */
	GHashTable *pinvoke_scopes;
	#endif

	/* The loader used to load this image */
	MonoImageLoader *loader;

	// Containers for MonoGenericParams associated with this image but not with any specific class or method. Created on demand.
	// This could happen, for example, for MonoTypes associated with TypeSpec table entries.
	MonoGenericContainer *anonymous_generic_class_container;
	MonoGenericContainer *anonymous_generic_method_container;

	gboolean weak_fields_inited;
	/* Contains 1 based indexes */
	GHashTable *weak_field_indexes;

	#ifdef ENABLE_METADATA_UPDATE
	/* List of MonoImages of deltas.  Parent image owns 1 refcount ref of the delta image */
	GList *delta_image;
	/* Tail of delta_image for fast appends */
	GList *delta_image_last;

	/* Metadata delta images only */
	uint32_t generation; /* global update ID that added this delta image */

	/* Maps MethodDef token indices to something. In base images a boolean
	 * flag that there's an update for the method; in delta images a
	 * pointer into the RVA of the delta IL */
	GHashTable *method_table_update;


	#endif

	/*
	 * No other runtime locks must be taken while holding this lock.
	 * It's meant to be used only to mutate and query structures part of this image.
	 */
	mono_mutex_t    lock;
	
	 */
}
#[repr(C)]
pub struct MonoProfilerCallContext
{
	/*
 	 * Must be the first field (the JIT relies on it). Only filled out if this
 	 * is a JIT frame; otherwise, zeroed.
 	 */
	context: MonoContext,
	/*
	 * A non-NULL MonoInterpFrameHandle if this is an interpreter frame.
	 */
	interp_frame: *const (),
	method: *const MonoMethod,
	/*
	 * Points to the return value for an epilogue context. For a prologue, this
	 * is set to NULL.
	 */
	return_value: *const (),
	/*
	 * Points to an array of addresses of stack slots holding the arguments.
	 */
	args: *const *const ()
}
#[repr(C)]
pub struct MonoContext
{
	regs: [usize; 15],
	ip: *const guint8,
	sp: *const *const (),
	fp: *const *const (),
}
#[repr(u8)]
pub enum MonoTypeNameFormat {
	IL,
	REFLECTION,
	FullName,
	AssemblyQualified,
	ReflectionQualified
}
#[repr(u8)]
pub enum MonoTypeEnum {
	END        = 0x00,   /* End of List */
	VOID       = 0x01,
	BOOLEAN    = 0x02,
	CHAR       = 0x03,
	I1         = 0x04,
	U1         = 0x05,
	I2         = 0x06,
	U2         = 0x07,
	I4         = 0x08,
	U4         = 0x09,
	I8         = 0x0a,
	U8         = 0x0b,
	R4         = 0x0c,
	R8         = 0x0d,
	STRING     = 0x0e,
	PTR        = 0x0f,   /* arg: <type> token */
	BYREF      = 0x10,   /* arg: <type> token */
	VALUETYPE  = 0x11,   /* arg: <type> token */
	CLASS      = 0x12,   /* arg: <type> token */
	VAR	     = 0x13,	 /* number */
	ARRAY      = 0x14,   /* type, rank, boundsCount, bound1, loCount, lo1 */
	GENERICINST= 0x15,	 /* <type> <type-arg-count> <type-1> \x{2026} <type-n> */
	TYPEDBYREF = 0x16,
	I          = 0x18,
	U          = 0x19,
	FNPTR      = 0x1b,	 /* arg: full method signature */
	OBJECT     = 0x1c,
	SZARRAY    = 0x1d,   /* 0-based one-dim-array */
	MVAR	     = 0x1e, /* number */
	CmodReqd = 0x1f,     /* arg: typedef or typeref token */
	CmodOpt = 0x20,      /* optional arg: typedef or typref token */
	INTERNAL   = 0x21,   /* CLR internal type */

	MODIFIER   = 0x40,   /* Or with the following types */
	SENTINEL   = 0x41,   /* Sentinel for varargs method signature */
	PINNED     = 0x45,   /* Local var that points to pinned object */

	ENUM       = 0x55    /* an enumeration */
}

pub struct MonoType
{
	pub r#union: MonoTypeUnion,
	_pd: [u8; 5]
	/*
	unsigned int attrs    : 16; /* param attributes or field flags */
	MonoTypeEnum type     : 8;
	unsigned int has_cmods : 1;
	unsigned int byref    : 1;
	unsigned int pinned   : 1;  /* valid when included in a local var signature */
	 */
}

pub union MonoTypeUnion
{
	pub klass: *const MonoClass, /* for VALUETYPE and CLASS */
	pub mtype: *const MonoType,   /* for PTR */
	pub array: *const MonoArrayType, /* for ARRAY */
	pub method: *const MonoMethodSignature,
	pub generic_param: *const MonoGenericParam, /* for VAR and MVAR */
	pub generic_class: *const MonoGenericClass /* for GENERICINST */
}
pub type MonoArrayType = ();
pub type MonoGenericClass = ();
pub type MonoGenericParam = ();
pub type MonoMethodSignature = ();
pub type MonoRuntimeGenericContext = ();
pub type MonoDomain = ();
pub type MonoClassField = ();