open Camlp4

let (@@) f a = f a
let (|>) a f = f a

module Make (AstFilters : Camlp4.Sig.AstFilters) =
struct
  open AstFilters

	type func = {
		name: string;
		ty: Ast.ctyp;
		f_name: string;
		f_type: string;
	}

	let gen =
		let module P = Camlp4.PreCast.Printers.OCaml in
		let rec _loc = Ast.Loc.ghost
		and cast_str_item ast =
			(Obj.magic ast : Camlp4.PreCast.Ast.str_item)
		(* and count_args = *)
		(* 	let rec loop n = function *)
		(* 		| <:ctyp< $t$ -> $ts$ >> -> loop (n+1) ts *)
		(* 		| <:ctyp< $t$ >> -> n *)
		(* 	in loop 0 *)
		(* and gen_arg_list s n = *)
		(* 	let rec loop i acc = match n with *)
		(* 		| i when i > n -> acc *)
		(* 		| i -> loop (i+1) ((s ^ (string_of_int i)) :: acc) *)
		(* 	in loop 1 [] *)
		in

	object(self)

		val expect_excs = (<:str_item<
			exception Invalid_expect ;
			exception Not_implemented >>)

		(* XXX: how will we get this name? *)
		val mutable mname = "Foo"

		val mutable funcs = []

		val hash_name = "e"

		method add_func fn ty =
			let f_name = "F_" ^ fn in
			let f_type = "T_" ^ fn in
			funcs <- {name = fn;
								ty = ty;
								f_name = f_name;
								f_type = f_type} :: funcs

		(* XXX: merge f_name and f_type methods *)
		method f_name =
			let mk_cons f = <:ctyp< $uid:f.f_name$ >> in
			let rec loop = function
				| []    -> <:ctyp< >>
				| f::fs -> <:ctyp< $mk_cons f$ | $loop fs$ >>
			in if funcs == []
				then <:str_item< >>
				else <:str_item< type f_name = [ $loop funcs$ ] >>

		method f_type =
			let mk_cons f = <:ctyp< $uid:f.f_type$ of $f.ty$ >> in
			let rec loop = function
				| []    -> <:ctyp< >>
				| f::fs -> <:ctyp< $mk_cons f$ | $loop fs$ >>
			in if funcs = []
				then <:str_item< >>
				else <:str_item< type f_type = [ $loop funcs$ ] >>

		method mock_sig mn =
			<:str_item< module type $"SIG_" ^ mn$ = module type of $uid:mn$ >>

		method gen_expect_funcs =
			let add_mock = <:str_item<
				value add_mock fn ty = Hashtbl.add $lid:hash_name$ fn ty >> in
			let fs = List.fold_left
				(fun st f ->
					<:str_item<
						$st$ ;
						value $lid:f.name$ = fun
							[ f -> add_mock $uid:f.f_name$ ( $uid:f.f_type$ f ) ] >>)
							(* [ f -> add_mock $uid:"F_"^f.name$ ( $uid:"T_"^f.name$ f ) ] >>) *)
				<:str_item< >>
				funcs
			in
			<:str_item< $add_mock$ ; $fs$ >>

		method module_expect = <:str_item<
			module Expect = struct
				$expect_excs$ ;
				$self#f_name$ ;
				$self#f_type$ ;
				value $lid:hash_name$ = Hashtbl.create 10 ;
				$self#gen_expect_funcs$ ;
			end
		>>

		method module_mock = <:str_item<
			module $mname$ = struct
				include $uid:mname$ ;
			end
		>>

		method module_outer =
			funcs <- List.rev funcs ;
			<:str_item<
			module $"Mock" ^ mname$ = struct
				$self#mock_sig mname$ ;
				$self#module_expect$ ;
				$self#module_mock$ ;
			end
		>>

		method print =
			let pp a = P.print_implem @@ cast_str_item a in
			pp <:str_item<
				$self#mock_sig mname$ ;
				$self#module_expect$ ;
				$self#module_mock$ ;
			>>

	end

	let sig_filter = function
		| <:sig_item< value $lid:fn$ : $typ:ty$ >> ->
				gen#add_func fn ty
		| _ -> ()
	;;

  register_sig_item_filter @@ fun sg ->
		List.iter sig_filter @@ Ast.list_of_sig_item sg [] ;
		gen#print ;
    let _loc = Ast.loc_of_sig_item sg in
		<:sig_item< >>
  ;;

end

module Id =
struct
  let name = "mocaml"
  let version = "0.1"
end

;;

let module M = Camlp4.Register.AstFilter(Id)(Make) in ()
