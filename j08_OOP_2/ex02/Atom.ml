(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Atom.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/25 11:40:08 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/25 13:52:17 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual atom na sy nb =
		object
		  initializer print_endline "Atom Ctor"
		  val _name = na
		  method name = _name
		  val _symbol = sy
		  method symbol = _symbol
		  val _atomic_number = nb
		  method atomic_number = _atomic_number
		  method to_string =
			"" ^ _name ^ "(" ^ _symbol ^ ")#"
			^ (string_of_int _atomic_number)
		  method equals (that:atom) = _atomic_number = that#atomic_number
		end

class hydrogen = object inherit atom "hydrogen" "H" 1 end
class helium = object inherit atom "helium" "He" 2 end
class carbon = object inherit atom "carbon" "C" 6 end
class nitrogen = object inherit atom "nitrogen" "N" 7 end
class oxygen = object inherit atom "oxygen" "O" 8 end
class fluorine = object inherit atom "fluorine" "F" 9 end
class sodium = object inherit atom "sodium" "Na" 11 end
class magnesium = object inherit atom "magnesium" "Mg" 12 end
class aluminum = object inherit atom "aluminum" "Al" 13 end
class silicon = object inherit atom "silicon" "Si" 14 end
class phosphorus = object inherit atom "phosphorus" "P" 15 end
class sulfur = object inherit atom "sulfur" "S" 16 end
class chlorine = object inherit atom "chlorine" "Cl" 17 end
class argon = object inherit atom "argon" "Ar" 18 end
class potassium = object inherit atom "potassium" "K" 19 end
class calcium = object inherit atom "calcium" "Ca" 20 end
