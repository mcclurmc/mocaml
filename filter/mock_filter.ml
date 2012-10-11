open Camlp4

let (@@) f a = f a
let (|>) a f = f a

module Make (AstFilters : Camlp4.Sig.AstFilters) =
struct
  open AstFilters

  (* let rec str_filter = function *)
	(*   | <:str_item< *)
	(* 			value $rec:r$ $lid:bid$ $args$ = $exps$ >> as si -> *)
	(* 	| si -> si *)
	(* ;; *)

	type func = { name: string;
	              args: Ast.ctyp list }

	(* So how do I make a filter that turns a sig_item into a str_item?
		 Let's see how p4_idl.ml does it. *)
	let rec sig_filter = function
		| <:sig_item< value $lid:bid$ : $typ:t$ >> as si ->
				print_endline "filtering..." ;
				si
		| si -> si
	;;

  AstFilters.register_sig_item_filter @@ fun si ->
    let _loc = Ast.loc_of_sig_item si in
		let sis = Ast.list_of_sig_item si [] in
		let _ = List.map sig_filter sis in
    <:sig_item< $list: sis$ >>
  ;;

end

module Id =
struct
  let name = "mocaml"
  let version = "0.1"
end

;;

let module M = Camlp4.Register.AstFilter(Id)(Make) in ()
