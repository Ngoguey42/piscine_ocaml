(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Reaction.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/25 15:29:10 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 19:39:34 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual reaction (st: (Molecule.molecule * int) list)
					   (re: (Molecule.molecule * int) list) =
		object
		  method virtual balance: reaction
		  method virtual get_start: (Molecule.molecule * int) list
		  method virtual get_result: (Molecule.molecule * int) list
		  method virtual balance: reaction
		  method virtual is_balanced: bool
		end
