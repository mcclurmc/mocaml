open Mock_foo

let _ =
	Expect.func1 (fun a b c -> "42") ;
	let n1 = Foo.func1 40 1 1 in
	Printf.printf "Answer: %s\n" n1 ;
