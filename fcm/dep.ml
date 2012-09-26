(* Thing that the SUT depends on, that we want to mock out *)
module Dependency =
struct
	let func1 () = 42
	let func2 () = print_endline "Production implementation"
	let func3 a b = Printf.sprintf "string: %s, int: %d" a b
end
