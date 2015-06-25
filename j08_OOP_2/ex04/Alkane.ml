(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Alkanes.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/25 14:29:07 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/25 16:33:53 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let list_of_n n =
  let rec helper elt n acc =
	match n with
	| 0				-> acc
	| _				-> helper elt (n - 1) (elt::acc)
  in
  if n < 0 then
	failwith "wrong arg"
  else
	(helper (new Atom.carbon) n []) @ (helper (new Atom.hydrogen) (2 * n + 2) [])

class alkane n =
object
  inherit Molecule.molecule "alkane" (list_of_n n)
  val _n = n
  method get_n = n
end
class methane = object inherit Molecule.molecule "methane" (list_of_n 1) end
class ethane = object inherit Molecule.molecule "ethane" (list_of_n 2) end
class propane = object inherit Molecule.molecule "propane" (list_of_n 3) end
class butane = object inherit Molecule.molecule "butane" (list_of_n 4) end
class pentane = object inherit Molecule.molecule "pentane" (list_of_n 5) end
class hexane = object inherit Molecule.molecule "hexane" (list_of_n 6) end
class heptane = object inherit Molecule.molecule "heptane" (list_of_n 7) end
class octane = object inherit Molecule.molecule "octane" (list_of_n 8) end

let make_start al =
  let pack_molecules accl mol =
	let sym = mol#formula in
	match accl with
	| (mol', n)::tl when sym = mol'#formula
	  -> ((mol' :> Molecule.molecule), n + 1)::tl
	| _                             		->
	   ((mol :> Molecule.molecule), 1)::accl
  in
  let al = List.sort (fun a b -> String.compare a#formula b#formula) al in
  (List.fold_left pack_molecules [] al) @ [(new Molecule.dioxygen, 1)]
											
class alkane_combustion (al: alkane list) =
object (self)
  val _start: (Molecule.molecule * int) list = make_start al

  val _result: (Molecule.molecule * int) list =
	[(new Molecule.carbon_dioxyde, 1); (new Molecule.water, 1)]
	  
  method private _get_balancing l =
	let rec helper l ((nc, nh, no) as acc) =
	  match l with
	  | []					-> acc
	  | (mol, n)::tl				->
		 let formula = mol#formula in
		 match formula with
		 | "O2"		-> helper tl (nc			,nh					,no + n*2)
		 | "CO2"	-> helper tl (nc + n*2		,nh					,no + n*2)
		 | "H2O"	-> helper tl (nc			,nh + n*2			,no + n)
		 | _		-> let n' = (mol :> alkane)#get_n in
					   helper tl (nc + n*n'		,nh + n*n'*2 + 2	,no)
	in
	helper l (0, 0, 0)
	
	
	(* method private _get_balancing l = *)
	(* 	let unpack_formula fo = *)
	(* 	  let len = String.length fo in *)
	(* 	  let is_upper = function *)
	(* 		| 'A'..'Z'	-> true *)
	(* 		| _			-> false *)
	(* 	  in *)
	(* 	  let is_digit = function *)
	(* 		| '0'..'9'	-> true *)
	(* 		| _			-> false *)
	(* 	  in *)
	(* 	  let rec elem_count elt i eltacc nbracc = *)
	(* 		if i = len && eltacc = elt then *)
	(* 		  1 *)
	(* 		else if i = len then *)
	(* 		  0 *)
	(* 		else *)
	(* 		  begin *)
	(* 			let c = String.get fo i in *)
	(* 			if is_upper c && eltacc = elt && nbracc = 0 then *)
	(* 			  1 *)
	(* 			else if is_upper c && eltacc = elt then *)
	(* 			  nbracc *)
	(* 			else if is_upper c then *)
	(* 			  elem_count elt (i + 1) (String.make 1 c) 0 *)
	(* 			else if  *)
	(* 		  end *)
	(* 		(\* if i = len then *\) *)
	(* 	  in *)
	(* 	in *)
	(* 	let foreach_pack (nc, nh, no) (mol, n) = *)
	(* 	  let (nc', nh', no') = unpack_formula mol#formula in *)
	(* 	  (nc + nc' * n, nh + nh' * n, no + no' * n) *)
	(* 	in *)
	(* 	List.fold_left foreach_pack (0, 0, 0) *)
	
	(* method is_balanced = *)
	
	(* method get_start = _start *)
	(* method get_result = _result *)
end
