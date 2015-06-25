(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Alkanes.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/25 14:29:07 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/25 14:55:24 by ngoguey          ###   ########.fr       *)
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

class alkane n = object inherit Molecule.molecule "alkane" (list_of_n n) end
class methane = object inherit Molecule.molecule "methane" (list_of_n 1) end
class ethane = object inherit Molecule.molecule "ethane" (list_of_n 2) end
class propane = object inherit Molecule.molecule "propane" (list_of_n 3) end
class butane = object inherit Molecule.molecule "butane" (list_of_n 4) end
class pentane = object inherit Molecule.molecule "pentane" (list_of_n 5) end
class hexane = object inherit Molecule.molecule "hexane" (list_of_n 6) end
class heptane = object inherit Molecule.molecule "heptane" (list_of_n 7) end
class octane = object inherit Molecule.molecule "octane" (list_of_n 8) end
  
