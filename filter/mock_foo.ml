module type FooSIG =
sig
	exception E1
	type 'a foo
  val func1 : int -> int -> int
  val func2 : string -> string -> string
  val funcN : int -> int -> int
end
module Foo : FooSIG =
struct
	exception E1
	type 'a foo = 'a list
  let rec func1 a b = a + b (* make sure we can handle this case *)
	and func2 a = function b -> a ^ b			(* and this case? *)
	let funcN = func1
end
module MockFoo =
struct
  module type SIG_Foo = module type of Foo
  module Expect =
  struct
		exception Invalid_expect
		exception Not_implemented
		type cond = Before | After
    type f_name = F_func1 | F_func2 | F_funcN (* Autogen *)
    let exp = Hashtbl.create 10
		let (cond : (f_name, cond) Hashtbl.t) = Hashtbl.create 10
		(* Autogen *)
    let func1 f = Hashtbl.add exp F_func1 (`Ty_func1 f)
    let func2 f = Hashtbl.add exp F_func2 (`Ty_func2 f)
    let funcN f = Hashtbl.add exp F_funcN (`Ty_funcN f)
  end
  module Foo : SIG_Foo =
  struct
		include Foo
    let func1 a b = try begin
  		let f = Expect.(Hashtbl.find exp F_func1) in
  		match f with
  			| `Ty_func1 f -> f a b
				| `Ty_func2 _ | `Ty_funcN _
					-> raise Expect.Invalid_expect
		end with _ ->
			(fun _ _ -> raise Expect.Not_implemented) a b
    and func2 a b = try begin
  		let f = Expect.(Hashtbl.find exp F_func2) in
  		match f with
  			| `Ty_func2 f -> f a b
  			| `Ty_func1 _ | `Ty_funcN _
					-> raise Expect.Invalid_expect
		end with _ ->
			(fun _ _ -> raise Expect.Not_implemented) a b
		and funcN a b = (* try *) begin
			if not Expect.(Hashtbl.mem exp F_funcN)
			then (fun _ _ -> raise Expect.Not_implemented) a b else
				let f = Expect.(Hashtbl.find exp F_funcN) in
				match f with
					| `Ty_funcN f -> f a b
					| `Ty_func2 _ | `Ty_func1 _
						-> raise Expect.Invalid_expect
		end (* with _ -> *)
	(* (fun _ _ -> raise Expect.Not_implemented) a b *)
  end
end

let _ =
	MockFoo.Expect.funcN (fun _ _ -> 42) ;
	MockFoo.Expect.funcN (fun a _ -> if a = 1 then raise Not_found else 42) ;
	MockFoo.Expect.func2 (fun _ _ -> "42") ;
	Printf.printf "func2 \"a\" \"b\" = \"%s\"\n" (MockFoo.Foo.func2 "a" "b") ;
	Printf.printf "funcN 1 2 = %d\n" (MockFoo.Foo.funcN 1 2) ;
