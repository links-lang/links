(* Measure performance *)


(*Previous  Up  Next
Module Gc

module Gc: sig .. end

Memory management control and statistics; finalised values.

type stat = {
   	minor_words : float; 	(*	Number of words allocated in the minor heap since the program was started. This number is accurate in byte-code programs, but only an approximation in programs compiled to native code.	*)
   	promoted_words : float; 	(*	Number of words allocated in the minor heap that survived a minor collection and were moved to the major heap since the program was started.	*)
   	major_words : float; 	(*	Number of words allocated in the major heap, including the promoted words, since the program was started.	*)
   	minor_collections : int; 	(*	Number of minor collections since the program was started.	*)
   	major_collections : int; 	(*	Number of major collection cycles completed since the program was started.	*)
   	heap_words : int; 	(*	Total size of the major heap, in words.	*)
   	heap_chunks : int; 	(*	Number of contiguous pieces of memory that make up the major heap.	*)
   	live_words : int; 	(*	Number of words of live data in the major heap, including the header words.	*)
   	live_blocks : int; 	(*	Number of live blocks in the major heap.	*)
   	free_words : int; 	(*	Number of words in the free list.	*)
   	free_blocks : int; 	(*	Number of blocks in the free list.	*)
   	largest_free : int; 	(*	Size (in words) of the largest block in the free list.	*)
   	fragments : int; 	(*	Number of wasted words due to fragmentation. These are 1-words free blocks placed between two live blocks. They are not available for allocation.	*)
   	compactions : int; 	(*	Number of heap compactions since the program was started.	*)
   	top_heap_words : int; 	(*	Maximum size reached by the major heap, in words.	*)
}

  The memory management counters are returned in a stat record.

  The total amount of memory allocated by the program since it was
  started is (in words) minor_words + major_words -
  promoted_words. Multiply by the word size (4 on a 32-bit machine, 8
  on a 64-bit machine) to get the number of bytes.

  val stat : unit -> stat
  
  Return the current values of the memory management counters in a
  stat record. This function examines every heap block to get the
  statistics.

  val quick_stat : unit -> stat
  
  Same as stat except that live_words, live_blocks, free_words,
  free_blocks, largest_free, and fragments are set to 0. This function
  is much faster than stat because it does not need to go through the
  heap.

  val counters : unit -> float * float * float

  Return (minor_words, promoted_words, major_words). This function is
  as fast at quick_stat.

  val print_stat : out_channel -> unit

  Print the current values of the memory management counters (in
  human-readable form) into the channel argument.

  val allocated_bytes : unit -> float

  Return the total number of bytes allocated since the program was
  started. It is returned as a float to avoid overflow problems with
  int on 32-bit machines.

*)

let measuring = ref false

let notify_gc () = 
  Sl_utility.debug ("Completing GC cycle")

let measure_diff obtain diff f a = 
  let start = obtain () in
  let result = f a in 
  let finish = obtain () in 
    result, diff finish start

let statdiff (major, minor, promoted) (major', minor', promoted') =
  (* Return the total amount of memory allocated *)
  (major +. minor -. promoted) -. (major' +. minor' -. promoted')

let time f           = measure_diff Sys.time (-.) f
let measure_memory f = measure_diff Gc.counters statdiff f

let write_time = 
  Printf.fprintf stderr "%.20s : %3f seconds taken\n"

let write_memory = 
  Printf.fprintf stderr "%.20s : %d words allocated\n"

let measure name (f : 'a -> 'b) (a : 'a) : 'b = 
  if !measuring then 
    let (result, time_taken), memory_allocated = measure_memory (time f) a in
      write_time name time_taken;
      write_memory name (int_of_float memory_allocated);
      flush stderr;
      result
  else
    f a
      
let _ = 
  Gc.create_alarm notify_gc
