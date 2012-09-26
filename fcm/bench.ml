module MyDepD = Dep.Dependency
	
let _ =

	let n = int_of_float 10e7 in

	let module MyDepI1 = (val !Globs.dependencyMod : Globs.DependencySIG) in
	let module MyDepI2 = (val Globs.dependencyMod2 : Globs.DependencySIG) in

	let myDepI3 = !Globs.dependencyMod in
	let module MyDepI3 = (val myDepI3 : Globs.DependencySIG) in

	let myDepI4 = if Globs.use_impl
		then (module Dep.Dependency : Globs.DependencySIG)
		else (module Dep.Dependency : Globs.DependencySIG) in
	let module MyDepI4 = (val myDepI4 : Globs.DependencySIG) in

	let myDepHash = Hashtbl.create 1 in
	Hashtbl.add myDepHash 0 (module Dep.Dependency : Globs.DependencySIG) ;
	let module MyDepI5 = (val (Hashtbl.find myDepHash 0) :  Globs.DependencySIG) in

	(* time for direct function call *)
	let time_direct_start = Sys.time () in
	for i = 1 to n do
		ignore (Dep.Dependency.func1 ())
	done ;
	let time_direct_end = Sys.time () in

	Printf.printf "Time for direct call:   %f\n"
		(time_direct_end -. time_direct_start) ;

	(* time for indirect function call through first class module *)
	let time_indirect_start = Sys.time () in
	for i = 0 to n do
		ignore (MyDepI3.func1 ())
	done ;
	let time_indirect_end = Sys.time () in

	Printf.printf "Time for indirect call, dereferenced once: %f\n"
		(time_indirect_end -. time_indirect_start) ;

	(* time for indirect function call through first class module, no ref *)
	let time_indirect_start = Sys.time () in
	for i = 0 to n do
		ignore (MyDepI2.func1 ())
	done ;
	let time_indirect_end = Sys.time () in

	Printf.printf "Time for indirect call, no reference: %f\n"
		(time_indirect_end -. time_indirect_start) ;

	(* time for indirect function call through first class module and if, no ref *)
	let time_indirect_start = Sys.time () in
	for i = 0 to n do
		ignore (MyDepI4.func1 ())
	done ;
	let time_indirect_end = Sys.time () in

	Printf.printf "Time for indirect call through if statement, no reference: %f\n"
		(time_indirect_end -. time_indirect_start) ;

	(* time for indirect function call through dereferenced first class module *)
	let time_indirect_start = Sys.time () in
	for i = 0 to n do
		ignore (MyDepI1.func1 ())
	done ;
	let time_indirect_end = Sys.time () in

	Printf.printf "Time for indirect call through reference: %f\n"
		(time_indirect_end -. time_indirect_start) ;

	(* time for indirect function call through hash table *)
	let time_indirect_start = Sys.time () in
	for i = 0 to n do
		ignore (MyDepI5.func1 ())
	done ;
	let time_indirect_end = Sys.time () in

	Printf.printf "Time for indirect call through hash table: %f\n"
		(time_indirect_end -. time_indirect_start) ;

