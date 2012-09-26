module type EXPECTATION =
sig
	exception No_return_set
	exception Violation
	type t
	val make : unit -> t
	val init : unit
end

module Expectation =
struct
	exception No_return_set
	exception Violation
	type expect_key = string
	type expect_val = string
	type t = (expect_key, expect_val) Hashtbl.t
	let make () = Hashtbl.create 10
end

(* This is the mock of Dependency. We'll write it out by hand to see
   what shape we'd like our framework to take. *)
module DependencyMock =
struct
	(* A type which defines the input signature of each function that
	   we're mocking. *)
	type funcs =
		| F_func1 of unit
		| F_func2 of unit
		| F_func3 of string * int
		| F_func3_lambda of (string -> int -> string)

	(* A type which defines the return values types for each function
	   we're mocking. XXX: how do we represent exceptions? *)
	type returns =
		| R_func1 of int
		| R_func2 of unit
		| R_func3 of string

	type const =
		| C_one_of
		| C_at_least_one_of

	type actions =
		| Func of funcs

	(* A hashtable of function calls and their return values *)
	let exp = (Hashtbl.create 10 : (funcs, returns) Hashtbl.t)

	type _ exp_gadt =
		| OneOf : actions -> actions exp_gadt
		| AtLeastOneOf : actions -> actions exp_gadt

	(* let init () = *)
	(* 	Hashtbl.add *)
	(* let expect = Expectation.make () *)

	(* Mocked functions *)
	let set_func1 a r = Hashtbl.add exp (F_func1 a) (R_func1 r)
	let func1 () =
		let f = F_func1 () in
		if Hashtbl.mem exp f
		then let r = Hashtbl.find exp f in
			 match r with
				 | R_func1 r -> r
				 | _ -> raise Expectation.No_return_set
		(* XXX: should record violations instead of excepting *)
		else raise Expectation.Violation

	let func2 () = print_endline "Mock implementation"

	(* A more interesting function to mock *)
	let set_func3 a b r = Hashtbl.add exp (F_func3 (a, b)) (R_func3 r)
	let set_func3_lambda: (string -> int -> string) -> unit =
		fun f -> ()
	let func3 a b =
		let f = F_func3 (a,b) in
		if Hashtbl.mem exp f
		then let r = Hashtbl.find exp f in
			 match r with
				 | R_func3 r -> r
				 | _ -> raise Expectation.No_return_set
		(* XXX: should record violations instead of excepting *)
		else raise Expectation.Violation

end

(* module type SUT_type = module type of Impl.SUT *)

(* module SUT_test = *)
(* 	functor (D : Globs.DependencySIG) -> *)
(* struct *)
(* 	include (Impl.SUT : SUT_type with module MyDep = D) *)
(* 	(\* module MyDep = D *\) *)
(* 	(\* include Impl.SUT *\) *)
(* end *)

(* Run the test, substituting our custom mock module for Dependency *)
let _ =
	print_endline "** Test **" ;
	Globs.dependencyMod := (module DependencyMock : Globs.DependencySIG) ;
	DependencyMock.set_func3 "a" 1 "MOCK string: a, int: 1" ;
	DependencyMock.set_func1 () 1 ;
	(* let module My_SUT = Impl.SUT(Dep.Dependency) in *)
	Impl.SUT.sut_func () ;
	let module My_SUT = Impl.SUT.Make(DependencyMock)(Hashtbl) in
	My_SUT.sut_func () ;

(* Expectation is an action with a list of conditions (at least once,
   only once, etc.). After an action on the mock is performed, it will
   be recorded. Either after each action, or at the end of the
   test, the conditions can be checked. *)
