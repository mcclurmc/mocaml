module MyDep = (val !Globs.dependencyMod : Globs.DependencySIG)

module type HASHTBL = module type of Hashtbl
(* SUT *)
module SUT =
struct
	module Make =
		functor (Dependency : Globs.DependencySIG) ->
		functor (H : HASHTBL) ->
	struct
	let sut_func () =
		print_endline "- sut_func() :" ;
		print_endline (Dependency.func3 "a" 1) ;
	end
	include Make(Dep.Dependency)(Hashtbl)
end

module SUT_orig =
struct
	open Dep
	let sut_func () =
		print_endline "- sut_func() :" ;
		print_endline (Dependency.func3 "a" 1) ;
end


(* print_endline "- Function-level definition:" ; *)
(* let module MyDep = (val !Globs.dependencyMod :
   Globs.DependencySIG) in *)
(* print_endline (MyDep.func3 "a" 1) ; *)
(* print_endline (string_of_int (MyDep.func1 ())) ; *)
