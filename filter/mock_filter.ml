open Camlp4

let (@@) f a = f a
let (|>) a f = f a

module Make (AstFilters : Camlp4.Sig.AstFilters) =
struct
  open AstFilters

	type func = {
		name: string;
	  args: Ast.ctyp list;
	}

	(* Things to generate:
	 * - module type SIG_<Module> = module type of <Module>
	 * - module Expect -- might want this to be an object
	 *   - exceptions, hashtable (can include this from a prototype
	 *     module, or a from a superclass
	 *   - type f_name = F_<func1> | F_<func2> | ...
	 *   - type f_type = T_<func1> of <ty_arg1> * <ty_arg2> ... | T_<func2> of ...
	 *   - type conditions = Before | After | ...
	 *   - let <func1> f = Hashtbl.add e F_<func1> (T_<func1> f, conditions list)
	 * - module <Module>
	 *   - include <Module>
	 *   - let <fun_name> <args list> = <some mock function impl>
	 *)

	(* module PP = Camlp4.Printers.OCaml.Make(Camlp4.PreCast.Syntax) *)
	(* let pp = new PP.printer ~comments: false () *)
	(* let print_expr = (new PP.printer ())#expr *)

	(* let print_str_item si = () *)

	(* let (str_pp, sig_pp) = Camlp4.Register.current_printer () *)

	module P = Camlp4.PreCast.Printers.OCaml

	(* So how do I make a filter that turns a sig_item into a str_item?
		 Let's see how p4_idl.ml does it... Okay, it doesn't do that. What
		 we'll do is make a filter that does a noop on the original mli
		 file, and as a side effect emits a new ml file based on what it
		 finds in the mli. *)
	let rec sig_filter = function
		| <:sig_item< value $lid:bid$ : $typ:ts$ >> as st ->
				let n = count_args ts in
				Printf.printf "filtering sig %s, %d args...\n" bid n ;
				let _loc = Ast.loc_of_sig_item st in
				let st = <:str_item< value $lid:bid$ x = () >> in
				P.print_implem @@ cast_str_item st ;
				empty_str_item _loc (* st *)
		| st ->
				print_endline "ignoring sig..." ;
				let _loc = Ast.loc_of_sig_item st in
				<:str_item< >>

	and count_args =
		let rec loop n = function
		| <:ctyp< $t$ -> $ts$ >> -> loop (n+1) ts
		| <:ctyp< $t$ >> -> n
		in loop 0

	and bar nm _loc =
		let _loc = AstFilters.Ast.Loc.ghost in
		let st = <:str_item< value foo = "Mike was here" >> in
		cast_str_item st

	and cast_str_item ast =
		(Obj.magic ast : Camlp4.PreCast.Ast.str_item)

	and empty_str_item _loc = <:str_item< >>
	;;

	let rec str_filter = function
		| <:str_item< value $lid:bid$ $args$ = $stmts$ >> as st ->
				Printf.printf "filtering str %s...\n" bid ; st
		| <:str_item< module type $lid:name$ = sig $sg$ end >> ->
				Printf.printf "filtering mod type %s...\n" name ;
				sig_filter sg
		| st -> print_endline "ignoring st..."; st
	;;

  register_sig_item_filter @@ fun si ->
    let _loc = Ast.loc_of_sig_item si in
		let sis = Ast.list_of_sig_item si [] in
		let _ = List.map sig_filter sis in
    <:sig_item< $list: sis$ >>
  ;;

	register_str_item_filter @@ fun st ->
		let _loc = Ast.loc_of_str_item st in
		let sts = Ast.list_of_str_item st [] in
		let sts = List.map str_filter sts in
		<:str_item< $list: sts$ >>
	;;

end

module Id =
struct
  let name = "mocaml"
  let version = "0.1"
end

;;

let module M = Camlp4.Register.AstFilter(Id)(Make) in ()
