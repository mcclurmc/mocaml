open Camlp4

let (@@) f a = f a
let (|>) a f = f a

module Make (AstFilters : Camlp4.Sig.AstFilters) =
struct
  open AstFilters

	type func = {
		name: string;
		ty: Ast.ctyp;
		argc: int;
		f_name: string;
		f_type: string;
	}

	let gen =
		let module P = Camlp4.PreCast.Printers.OCaml in
		let rec _loc = Ast.Loc.ghost
		and cast_str_item ast =
			(Obj.magic ast : Camlp4.PreCast.Ast.str_item)
		and count_args =
			let rec loop n = function
				| <:ctyp< $t$ -> $ts$ >> -> loop (n+1) ts
				| <:ctyp< $t$ >> -> n
			in loop 0
		and fold_str f st = List.fold_left
			(fun st x -> <:str_item< $st$ ; $f x$ >>) st
		and fold_str1 f = fold_str f <:str_item< >>
		in

	object(self)

		val expect_excs = (<:str_item<
			exception Invalid_expect ;
			exception Not_implemented >>)

		val mutable mod_name = ""
		val mutable mock_filename = ""
		val mutable funcs = []
		val hash_name = "e"

		method mock_filename = mock_filename

		method set_module_name _loc =
			let upper_first str =
				let c = Char.uppercase str.[0] in
				str.[0] <- c in
			let name =
				let fn = Ast.Loc.file_name _loc in
				let i = String.index fn '.' in
				String.sub fn 0 i in
			mock_filename <- ("mock_" ^ name ^ ".ml") ;
			upper_first name ;
			mod_name <- name

		method add_func name ty =
			let f_name = "F_" ^ name in
			let f_type = "T_" ^ name in
			let argc = count_args ty in
			funcs <- {name; ty; argc; f_name; f_type} :: funcs

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

		method mock_sig_name mn = String.uppercase (mn ^ "_SIG")

		method mock_sig mn =
			<:str_item< module type $self#mock_sig_name mn$ =
				module type of $uid:mn$ >>

		method gen_expect_funcs =
			let add_mock = <:str_item<
				value add_mock fn ty = Hashtbl.add $lid:hash_name$ fn ty >> in
			let fs = fold_str1
				(fun f -> <:str_item< value $lid:f.name$ = fun
					[ f -> add_mock $uid:f.f_name$ ( $uid:f.f_type$ f ) ] >>)
				funcs
			in
			<:str_item< $add_mock$ ; $fs$ >>

		method gen_mock_funcs =
			let a i = "a"^(string_of_int i) in
			let build_fun n e =
				let rec loop e = function
					| 0 -> <:expr< $e$ >>
					| i -> loop <:expr< fun $lid:(a i)$ -> $e$ >> (i-1)
				in loop e n
			in
			let build_app n f =
				let rec loop e = function
					| i when i > n -> e
					| i -> loop <:expr< $e$ $lid:(a i)$ >> (i+1)
				in loop <:expr< f >> 1
			in
			let lookup_mock = <:str_item<
				value lookup_mock =
					fun [ f ->
						let open Expect in
								if not (Hashtbl.mem e f)
								then raise Not_implemented
								else Hashtbl.find e f ]
					>>
			in
			fold_str (fun f -> <:str_item< value $lid:f.name$ =
					$build_fun f.argc
						(<:expr< let open Expect in
												 let f = lookup_mock $uid:f.f_name$ in
												 match f with
														 [ $uid:f.f_type$ f -> $build_app f.argc "f"$
														 | _ -> raise Invalid_expect ] >> )$ >> )
				lookup_mock funcs

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
				module $mod_name$ : $uid:self#mock_sig_name mod_name$ =
				struct
					include $uid:mod_name$ ;
					$self#gen_mock_funcs$ ;
				end
			>>

		method module_outer =
			funcs <- List.rev funcs ;
			<:str_item<
					$self#mock_sig mod_name$ ;
					$self#module_expect$ ;
					$self#module_mock$ ;
			>>

		method print =
			let pp a = P.print_implem
				~output_file:self#mock_filename
				(cast_str_item a)
			in pp self#module_outer ;
	end

let sig_filter = function
	| <:sig_item< value $lid:fn$ : $typ:ty$ >> ->
			gen#add_func fn ty
	| _ -> ()
;;

register_sig_item_filter @@ fun sg ->
  let _loc = Ast.loc_of_sig_item sg in
	gen#set_module_name _loc ;
	List.iter sig_filter (Ast.list_of_sig_item sg []) ;
	gen#print ;
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
