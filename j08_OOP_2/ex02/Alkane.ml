(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Alkanes.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/25 14:29:07 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 19:37:11 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let name_of_int = function
  | 1-> "Methane"
  | 2-> "Ethane"
  | 3-> "Propane"
  | 4-> "Butane"
  | 5-> "Pentane"
  | 6-> "Hexane"
  | 7-> "Heptane"
  | 8-> "Octane"
  | 9-> "Nonane"
  | 10-> "Decane"
  | 11-> "Undecane"
  | 12-> "Dodecane"
  | 16-> "Hexadecane"
  | 20-> "Icosane"
  | 30-> "Triacontane"
  | 40-> "Tetracontane"
  | 50-> "Pentacontane"
  | 60-> "Hexacontane"
  | _-> "Alkane"

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

class alkane n = object
  inherit Molecule.molecule (name_of_int n) (list_of_n n)
end
class methane = object inherit Molecule.molecule "methane" (list_of_n 1) end
class ethane = object inherit Molecule.molecule "ethane" (list_of_n 2) end
class propane = object inherit Molecule.molecule "propane" (list_of_n 3) end
class butane = object inherit Molecule.molecule "butane" (list_of_n 4) end
class pentane = object inherit Molecule.molecule "pentane" (list_of_n 5) end
class hexane = object inherit Molecule.molecule "hexane" (list_of_n 6) end
class heptane = object inherit Molecule.molecule "heptane" (list_of_n 7) end
class octane = object inherit Molecule.molecule "octane" (list_of_n 8) end
  
