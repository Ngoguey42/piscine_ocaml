(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Molecule.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/25 12:09:48 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/25 13:29:54 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let formula_of_list l =
  let pack_atoms accl elt =
	let sym = elt#symbol in
	match accl with
	| (sym', n)::tl when sym' = sym	-> (sym', n + 1)::tl
	| _								-> (sym, 1)::accl
  in
  let sort_packs (asym, _) (bsym, _) =
	match asym, bsym with
	| "C", _				-> 1
	| _, "C"				-> -1
	| "H", _				-> 1
	| _, "H"				-> -1
	| _, _					-> String.compare asym bsym
  in
  let rec string_of_packs l acc =
	match l with
	| []					-> acc
	| (sym, n)::tl			-> string_of_packs tl (acc ^ sym ^ string_of_int n)
  in
  let l = List.sort (fun a b -> String.compare a#symbol b#symbol) l in
  let l = List.fold_left pack_atoms [] l in
  let l = List.sort sort_packs l in
  string_of_packs l ""

class virtual molecule na al =
		object (self)
		  initializer print_endline "Molecule Ctor"			
		  val _name = na
		  method name = _name
		  val list : Atom.atom list = al
		  val _formula = formula_of_list al
		  method formula = _formula
		  method to_string =
			"" ^ _name ^ "(" ^ _formula ^ ")"
		end

let h = new Atom.hydrogen
let o = new Atom.oxygen
let c = new Atom.carbon
		  
class water = object inherit molecule "water" [h; o; h] end
class carbon_dioxyde = object inherit molecule "carbon_dioxyde" [c; o; o] end
