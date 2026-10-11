/*
 *
 * Copyright (c) 2026 Carbon Community, under the GNU v3 license rights
 *
 */

use std::collections::hash_map::Entry;
use std::time::{Duration, Instant};

use arrayvec::ArrayVec;
use rustc_hash::FxHashMap;

use crate::mono::{MonoClass, MonoMethod};

#[derive(Default, Copy, Clone, Debug, PartialEq)]
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

	fn is_empty(&self) -> bool
	{
		*self == MethodResult::default()
	}
}

#[derive(Default, Copy, Clone, Debug, PartialEq)]
pub struct MemoryResult
{
	pub allocations: u64,
	pub total_alloc_size: u64
}

impl MemoryResult
{
	pub fn merge_from(&mut self, input: &MemoryResult)
	{
		self.allocations += input.allocations;
		self.total_alloc_size += input.total_alloc_size;
	}
}

pub fn percentage_of(part: Duration, total: Duration) -> f64
{
	if total.is_zero() {
		return 0f64;
	}
	part.as_secs_f64() / total.as_secs_f64() * 100f64
}

struct Frame
{
	method: *const MonoMethod,
	entry: usize,
	own_allocations: u64,
	total_allocations: u64,
	timing: Option<FrameTiming>
}

struct FrameTiming
{
	started: Instant,
	children: Duration
}

struct MethodEntry
{
	method: *const MonoMethod,
	active_frames: u32,
	last_walk: u64,
	result: MethodResult
}

#[derive(Default)]
pub struct CallRecorder
{
	stack: Vec<Frame>,
	entries: Vec<MethodEntry>,
	entry_index: FxHashMap<*const MonoMethod, usize>,
	walk: u64
}

impl CallRecorder
{
	fn entry(&mut self, method: *const MonoMethod) -> usize
	{
		match self.entry_index.entry(method) {
			Entry::Occupied(entry) => *entry.get(),
			Entry::Vacant(entry) => {
				let index = self.entries.len();
				self.entries.push(MethodEntry {
					method,
					active_frames: 0,
					last_walk: 0,
					result: MethodResult::default()
				});
				entry.insert(index);
				index
			}
		}
	}

	pub fn enter(&mut self, method: *const MonoMethod, started: Option<Instant>)
	{
		let entry = self.entry(method);
		self.entries[entry].active_frames += 1;
		self.stack.push(Frame {
			method,
			entry,
			own_allocations: 0,
			total_allocations: 0,
			timing: started.map(|started| FrameTiming { started, children: Duration::ZERO })
		});
	}

	pub fn exit(&mut self, method: *const MonoMethod, exceptional: bool, now: impl FnOnce() -> Instant)
	{
		let Some(index) = self.stack.iter().rposition(|frame| frame.method == method) else {
			return;
		};
		self.discard_frames_from(index + 1);
		let Some(frame) = self.stack.pop() else {
			return;
		};

		let total_time = frame.timing.as_ref().map(|timing| now().duration_since(timing.started));
		let entry = &mut self.entries[frame.entry];
		entry.active_frames -= 1;
		entry.result.calls += 1;
		entry.result.own_allocations += frame.own_allocations;
		if exceptional {
			entry.result.total_exceptions += 1;
		}
		if let (Some(total_time), Some(timing)) = (total_time, &frame.timing) {
			entry.result.own_time += total_time.saturating_sub(timing.children);
		}
		if entry.active_frames == 0 {
			entry.result.total_allocations += frame.total_allocations;
			entry.result.total_time += total_time.unwrap_or_default();
		}

		if let Some(parent) = self.stack.last_mut() {
			parent.total_allocations += frame.total_allocations;
			if let (Some(timing), Some(total_time)) = (&mut parent.timing, total_time) {
				timing.children += total_time;
			}
		}
	}

	fn discard_frames_from(&mut self, len: usize)
	{
		for frame in self.stack.drain(len..) {
			self.entries[frame.entry].active_frames -= 1;
		}
	}

	pub fn exception_thrown(&mut self)
	{
		if let Some(frame) = self.stack.last() {
			self.entries[frame.entry].result.own_exceptions += 1;
		}
	}

	pub fn allocation(&mut self, size: u64)
	{
		if let Some(frame) = self.stack.last_mut() {
			frame.own_allocations += size;
			frame.total_allocations += size;
		}
	}

	pub fn begin_allocation_walk(&mut self)
	{
		self.walk += 1;
	}

	pub fn walked_frame(&mut self, method: *const MonoMethod, size: u64, own: bool)
	{
		let walk = self.walk;
		let index = self.entry(method);
		let entry = &mut self.entries[index];
		if own {
			entry.result.own_allocations += size;
		}
		if entry.last_walk != walk {
			entry.last_walk = walk;
			entry.result.total_allocations += size;
		}
	}

	pub fn collect(&mut self, report: &mut FxHashMap<*const MonoMethod, MethodResult>)
	{
		for entry in &mut self.entries {
			if entry.result.is_empty() {
				continue;
			}
			report.entry(entry.method).or_default().merge_from(&entry.result);
			entry.result = MethodResult::default();
		}
	}

	pub fn clear(&mut self)
	{
		self.stack.clear();
		self.entries.clear();
		self.entry_index.clear();
	}
}

#[derive(Default)]
pub struct MemoryRecorder
{
	classes: FxHashMap<*const MonoClass, MemoryResult>
}

impl MemoryRecorder
{
	pub fn allocation(&mut self, class: *const MonoClass, size: u64)
	{
		let result = self.classes.entry(class).or_default();
		result.allocations += 1;
		result.total_alloc_size += size;
	}

	pub fn collect(&mut self, report: &mut FxHashMap<*const MonoClass, MemoryResult>)
	{
		for (class, result) in self.classes.drain() {
			report.entry(class).or_default().merge_from(&result);
		}
	}

	pub fn clear(&mut self)
	{
		self.classes.clear();
	}
}

pub struct AllocationScopes
{
	scopes: ArrayVec<u64, 16>,
	overflow: u32
}

impl AllocationScopes
{
	pub const fn new() -> Self
	{
		AllocationScopes {
			scopes: ArrayVec::new_const(),
			overflow: 0
		}
	}

	pub fn reset(&mut self)
	{
		self.scopes.clear();
		self.overflow = 0;
	}

	pub fn push(&mut self) -> bool
	{
		match self.scopes.try_push(0) {
			Ok(_) => true,
			Err(_) => {
				self.overflow += 1;
				false
			}
		}
	}

	pub fn pop(&mut self) -> u64
	{
		if self.overflow > 0 {
			self.overflow -= 1;
			return 0;
		}
		self.scopes.pop().unwrap_or_default()
	}

	pub fn allocation(&mut self, size: u64)
	{
		if self.overflow == 0 {
			if let Some(scope) = self.scopes.last_mut() {
				*scope += size;
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	fn method(id: usize) -> *const MonoMethod
	{
		id as *const MonoMethod
	}

	fn class(id: usize) -> *const MonoClass
	{
		id as *const MonoClass
	}

	fn collect(recorder: &mut CallRecorder) -> FxHashMap<*const MonoMethod, MethodResult>
	{
		let mut report = FxHashMap::default();
		recorder.collect(&mut report);
		report
	}

	#[test]
	fn nested_calls_propagate_allocations_to_the_caller()
	{
		let mut recorder = CallRecorder::default();
		recorder.enter(method(1), None);
		recorder.allocation(10);
		recorder.enter(method(2), None);
		recorder.allocation(5);
		recorder.exit(method(2), false, Instant::now);
		recorder.exit(method(1), false, Instant::now);

		let report = collect(&mut recorder);
		assert_eq!(report[&method(1)].calls, 1);
		assert_eq!(report[&method(1)].own_allocations, 10);
		assert_eq!(report[&method(1)].total_allocations, 15);
		assert_eq!(report[&method(2)].own_allocations, 5);
		assert_eq!(report[&method(2)].total_allocations, 5);
	}

	#[test]
	fn recursion_counts_totals_once()
	{
		let base = Instant::now();
		let mut recorder = CallRecorder::default();
		recorder.enter(method(1), Some(base));
		recorder.enter(method(1), Some(base + Duration::from_millis(1)));
		recorder.allocation(7);
		recorder.exit(method(1), false, || base + Duration::from_millis(3));
		recorder.exit(method(1), false, || base + Duration::from_millis(4));

		let result = collect(&mut recorder)[&method(1)];
		assert_eq!(result.calls, 2);
		assert_eq!(result.total_allocations, 7);
		assert_eq!(result.own_allocations, 7);
		assert_eq!(result.total_time, Duration::from_millis(4));
		assert_eq!(result.own_time, Duration::from_millis(4));
	}

	#[test]
	fn own_time_excludes_children()
	{
		let base = Instant::now();
		let mut recorder = CallRecorder::default();
		recorder.enter(method(1), Some(base));
		recorder.enter(method(2), Some(base + Duration::from_millis(2)));
		recorder.exit(method(2), false, || base + Duration::from_millis(7));
		recorder.exit(method(1), false, || base + Duration::from_millis(10));

		let report = collect(&mut recorder);
		assert_eq!(report[&method(1)].total_time, Duration::from_millis(10));
		assert_eq!(report[&method(1)].own_time, Duration::from_millis(5));
		assert_eq!(report[&method(2)].own_time, Duration::from_millis(5));
	}

	#[test]
	fn unmatched_exit_keeps_running_frames()
	{
		let mut recorder = CallRecorder::default();
		recorder.enter(method(1), None);
		recorder.exit(method(2), false, Instant::now);
		recorder.allocation(3);
		recorder.exit(method(1), false, Instant::now);

		let report = collect(&mut recorder);
		assert_eq!(report[&method(1)].calls, 1);
		assert_eq!(report[&method(1)].own_allocations, 3);
		assert!(!report.contains_key(&method(2)));
	}

	#[test]
	fn missed_exit_is_discarded_by_the_callers_exit()
	{
		let mut recorder = CallRecorder::default();
		recorder.enter(method(1), None);
		recorder.enter(method(2), None);
		recorder.exit(method(1), false, Instant::now);
		recorder.enter(method(2), None);
		recorder.allocation(4);
		recorder.exit(method(2), false, Instant::now);

		let report = collect(&mut recorder);
		assert_eq!(report[&method(1)].calls, 1);
		assert_eq!(report[&method(2)].calls, 1);
		assert_eq!(report[&method(2)].total_allocations, 4);
	}

	#[test]
	fn exceptions_count_on_the_throwing_and_exiting_frames()
	{
		let mut recorder = CallRecorder::default();
		recorder.enter(method(1), None);
		recorder.enter(method(2), None);
		recorder.exception_thrown();
		recorder.exit(method(2), true, Instant::now);
		recorder.exit(method(1), false, Instant::now);

		let report = collect(&mut recorder);
		assert_eq!(report[&method(2)].own_exceptions, 1);
		assert_eq!(report[&method(2)].total_exceptions, 1);
		assert_eq!(report[&method(1)].total_exceptions, 0);
	}

	#[test]
	fn walked_allocations_count_recursive_methods_once()
	{
		let mut recorder = CallRecorder::default();
		recorder.begin_allocation_walk();
		recorder.walked_frame(method(1), 8, true);
		recorder.walked_frame(method(2), 8, false);
		recorder.walked_frame(method(1), 8, false);

		let report = collect(&mut recorder);
		assert_eq!(report[&method(1)].own_allocations, 8);
		assert_eq!(report[&method(1)].total_allocations, 8);
		assert_eq!(report[&method(2)].own_allocations, 0);
		assert_eq!(report[&method(2)].total_allocations, 8);
	}

	#[test]
	fn collect_keeps_the_stack_and_resets_results()
	{
		let mut recorder = CallRecorder::default();
		recorder.enter(method(1), None);
		recorder.enter(method(2), None);
		recorder.exit(method(2), false, Instant::now);
		assert_eq!(collect(&mut recorder)[&method(2)].calls, 1);

		recorder.exit(method(1), false, Instant::now);
		let report = collect(&mut recorder);
		assert_eq!(report[&method(1)].calls, 1);
		assert!(!report.contains_key(&method(2)));
	}

	#[test]
	fn memory_recorder_merges_into_the_report()
	{
		let mut first = MemoryRecorder::default();
		let mut second = MemoryRecorder::default();
		first.allocation(class(1), 16);
		second.allocation(class(1), 32);
		second.allocation(class(2), 8);

		let mut report = FxHashMap::default();
		first.collect(&mut report);
		second.collect(&mut report);
		assert_eq!(report[&class(1)], MemoryResult { allocations: 2, total_alloc_size: 48 });
		assert_eq!(report[&class(2)], MemoryResult { allocations: 1, total_alloc_size: 8 });
	}

	#[test]
	fn allocation_scopes_ignore_allocations_after_overflow()
	{
		let mut scopes = AllocationScopes::new();
		for _ in 0..16 {
			assert!(scopes.push());
		}
		assert!(!scopes.push());
		scopes.allocation(5);
		assert_eq!(scopes.pop(), 0);
		scopes.allocation(5);
		assert_eq!(scopes.pop(), 5);
	}

	#[test]
	fn percentage_of_an_empty_session_is_zero()
	{
		assert_eq!(percentage_of(Duration::from_millis(5), Duration::ZERO), 0f64);
		assert_eq!(percentage_of(Duration::from_millis(500), Duration::from_secs(2)), 25f64);
	}
}
