(* An example of manually mocking a module. This is really a more of a
	 stub, not a mock, because it doesn't provide anything other than a
	 simple counter for the mocked function. We can't specify limits on
	 the number of times the function is called, or say that must be
	 called before some other function. Regardless, this is a simple way
	 to replace a dependency in a system under test with a fake
	 dependency that is easier to test. *)

module type DOC_SIG = sig
	val f : int -> int
end

module DOC =
struct
	let f a = a
end

module SUT =
(* Here we functorize the original system under test, so that we can
	 replace the depended on component with something of our own
	 choosing. We include applied functor in the module SUT so that
	 callers can more easily use SUT with the original dependency. *)
struct module Make(DOC : DOC_SIG) = struct
	open DOC
	let do_it a = f a
end
			 include Make(DOC)
end

module Test =
struct
	let f_count = ref 0
	module M : DOC_SIG =
	struct
		include DOC
		let f _ = incr f_count ; 42
	end
	let test () =
		let module Test_SUT = SUT.Make(M) in
		Printf.printf "Test SUT: %d\n" (Test_SUT.do_it 2) ;        (* 42 *)
		Printf.printf "Real SUT: %d\n" (SUT.do_it 2) ;             (* 2 *)
		Printf.printf "Test SUT: %d\n" (Test_SUT.do_it 2) ;        (* 42 *)
		Printf.printf "Mock function called %d times\n" !f_count ; (* 2 *)
end

let _ = Test.test ()
