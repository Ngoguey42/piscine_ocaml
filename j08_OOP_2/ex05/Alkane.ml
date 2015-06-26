(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Alkanes.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/25 14:29:07 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 19:35:07 by ngoguey          ###   ########.fr       *)
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

											
(* EX04 *)
(* GET TUPLE OF ATOMS QUANTITY IN L *)
let get_balancing l =
  let parse_alkane_formula formula =
	let len = String.length formula in
	let rec helper i nbracc =
	  if i = len then
		0
	  else
		let c = String.get formula i in
		match c with
		| 'H' when nbracc = 0	-> 1
		| 'H'					-> nbracc
		| 'C'					-> helper (i + 1) nbracc
		| _						->
		   (int_of_char c - int_of_char '0') + nbracc * 10
	in
	helper 0 0
  in
  let rec helper l ((nc, nh, no) as acc) =
	match l with
	| []				-> acc
	| (mol, n)::tl	->
	   let formula = mol#formula in
	   match formula with
	   | "O2"		-> helper tl (nc			,nh					,no + n*2)
	   | "CO2"	-> helper tl (nc + n		,nh					,no + n*2)
	   | "H2O"	-> helper tl (nc			,nh + n*2			,no + n)
	   | _		-> let n' = parse_alkane_formula mol#formula in
				   helper tl (nc + n*n'		,nh + n*n'*2 + 2*n	,no)
  in
  helper l (0, 0, 0)

(* DEBUG/TEST FUNCTION *)
let to_string_alkane_reac st re =
  let rec helper l acc =
	match l with
	| []				-> acc
	| (mol, n)::tl	->
	   helper tl (acc ^ string_of_int n ^ "(" ^ mol#formula ^ ")")
  in
  let (nc, nh, no) as a = get_balancing st in
  let (nc', nh', no') as b = get_balancing re in
  let f = string_of_int in
  f nc ^ "C " ^ f nh ^ "H " ^ f no ^ "O = "
  ^ f nc' ^ "C " ^ f nh' ^ "H " ^ f no' ^ "O\n"
  ^ (helper st "") ^ " = " ^ (helper re "")
  ^ "\nis_balanced: " ^ string_of_bool (a = b)
									   
(* DUMMY CLASS FOR NEW INSTANCIATION *)
class newalkane_combusion (st: (Molecule.molecule * int) list)
						  (re: (Molecule.molecule * int) list) =
object (self)
  inherit Reaction.reaction st re
  method get_start: (Molecule.molecule * int) list = st
  method get_result: (Molecule.molecule * int) list = re
  method balance: Reaction.reaction = (self :> Reaction.reaction)
  method is_balanced: bool = true
  method to_string = to_string_alkane_reac st re
end

(* REQUIRED CLASS   *)
class alkane_combustion (al: alkane list) =
object (self)

  (* LISTS SAVE *)
  val _start: (Molecule.molecule * int) list = make_start al
  val _result: (Molecule.molecule * int) list =
	[(new Molecule.carbon_dioxyde, 1); (new Molecule.water, 1)]
	  
  (* REQUIRED METHOD #1 *)
  method is_balanced =
	(get_balancing _start) = (get_balancing _result)
							   
  (* REQUIRED METHOD #2 *)
  method get_start =
	if self#is_balanced then
	  _start
	else
	  failwith "is not balanced"
			   
  (* REQUIRED METHOD #3 *)
  method get_result =
	if self#is_balanced then
	  _result
	else
	  failwith "is not balanced"

  (* REQUIRED INHERITANCE *)
  inherit Reaction.reaction	[] []

  (* REQUIRED METHOD #4 *)
  (* method balance = *)
  method balance : Reaction.reaction =
	(* BALANCE TREE WITH GIVEN DATAS *)
	let balance_left fact ndiox =
	  let rec helper l =
		match l with
		| []									-> []
		| (mol, _)::tl when mol#formula = "O2"	-> (mol, ndiox)::(helper tl)
		| (mol, n)::tl							-> (mol, n * fact)::(helper tl)
	  in
	  helper _start
	in
	(* PPCM (Least common multiple) *)
	let rec ppcm a b fa fb =
	  let a' = a * fa in
	  let b' = b * fb in
	  if a' = b' then a'
	  else if a' < b' then ppcm a b (fa + 1) fb
	  else ppcm a b fa (fb + 1)
	in
	let (nc, nh, _) as lbal = get_balancing _start in
	let rbal = get_balancing _result in
	if lbal <> rbal && nc > 0 then
	  begin
		let ncdiox = ppcm 4 nc 1 1 in
		let fact = ncdiox / nc in
		let nwater = nh * fact / 2 in
		let ndiox = (nwater + ncdiox * 2) / 2 in
		let newleft = balance_left fact ndiox in
		let newright = [(new Molecule.carbon_dioxyde, ncdiox);
						(new Molecule.water, nwater)] in
		new newalkane_combusion newleft newright
	  end
	else
	  new newalkane_combusion _start _result

  (* EX05 *)
  method get_incomplete_results : (int * (Molecule.molecule * int) list) list =
	(* CREATE THE RIGHT PART WITH GIVEN DATAS *)
	let new_right no nc nwat =
	  let newmol n mol =
		match n with
		| 0				-> []
		| _				-> [(mol, n)]
	  in
	  let nco2 = no / 2 in
	  let nco = no - nco2 * 2 in
	  let nc = nc - nco2 - nco in
	  newmol nco2 new Molecule.carbon_dioxyde
	  @ newmol nco new Molecule.carbon_monoxyde
	  @ newmol nc new Molecule.soot
	  @ newmol nwat new Molecule.water
	in
	
	let (nc, nh, _) = get_balancing _start in
	if nc > 0 then
	  begin
		let nwater = nh / 2 in
		let rec foreach_ndiox ndiox acc =
		  if ndiox < 0 || ndiox * 2 - nwater < 0 then
			acc
		  else
			foreach_ndiox (ndiox - 1)
						  ((ndiox, (new_right (ndiox * 2 - nwater) nc nwater))::acc)
		in
		foreach_ndiox (nwater / 2 + nc) []
	  end
	else
	  failwith "no alkane on left part"
			   
  method to_string =
	to_string_alkane_reac _start _result
end
