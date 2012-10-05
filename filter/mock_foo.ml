module Foo =
struct
	let func1 a b = a + b
	let func2 a b = a ^ b
	let funcN a b = a - b
end
module MockFoo =
struct
  module type SIG_Foo = module type of Foo
  module Expect =
  struct
    type f_name = F_func1 | F_func2 | F_funcN
	exception Invalid_expect
	exception Not_implemented
    let exp = Hashtbl.create 10
    let func1 f = Hashtbl.add exp F_func1 (`Ty_func1 f)
    let func2 f = Hashtbl.add exp F_func2 (`Ty_func2 f)
    let funcN f = Hashtbl.add exp F_funcN (`Ty_funcN f)
  end
  module Foo : SIG_Foo =
  struct
    let func1 a b = try begin
  		let f = Expect.(Hashtbl.find exp F_func1) in
  		match f with
  			| `Ty_func1 f -> f a b
			| `Ty_func2 _ | `Ty_funcN _
				-> raise Expect.Invalid_expect
  	end with _ -> raise Expect.Not_implemented
    and func2 a b = try begin
  		let f = Expect.(Hashtbl.find exp F_func2) in
  		match f with
  			| `Ty_func2 f -> f a b
  			| `Ty_func1 _ | `Ty_funcN _
				-> raise Expect.Invalid_expect
  	end with _ -> raise Expect.Not_implemented
	and funcN : int -> int -> int = fun a b -> try begin
  		let f = Expect.(Hashtbl.find exp F_func2) in
  		match f with
			| `Ty_funcN f -> f a b
  			| `Ty_func2 _ | `Ty_func1 _ 
				-> raise Expect.Invalid_expect
  	end with _ -> raise Expect.Not_implemented
  end
(*   let expect = *)
(*   object *)
(*   	  val exp = Hashtbl.create 10 *)
(*       method func1 f = Hashtbl.add exp `F_func1 (`Ty_func1 f) *)
(*   end *)
(* module Foo2 : SIG_Foo = *)
(* struct *)
(* end *)
end
