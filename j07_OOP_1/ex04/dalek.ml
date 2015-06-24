(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   dalek.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 16:19:52 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/24 16:46:00 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)
let genname =
  Random.self_init ();
  "Dalek" ^ String.init 3 (fun i ->
						   match i with
						   | 0	-> char_of_int(int_of_char 'A' + Random.int(26))
						   | _	-> char_of_int(int_of_char 'a' + Random.int(26)))

class dalek =
object 
  initializer print_endline "dalek CTOR"
  val name = genname
  val hp = 100
  val mutable shield = true
  method to_string =
	name
	^ ": " ^ (string_of_int hp) ^ "hp"
	^ ": shield_is_" ^ (string_of_bool shield)
  method talk =
	print_endline
	  [|"Explain! Explain!"
	  ; "Exterminate! Exterminate!"
	  ; "I obey!"
	  ; "You are the Doctor! You are the enemy of the Daleks!"|].(Random.int(4))
  method exterminate (p: People.people) =
	p#die;
	match shield with
	| true	-> shield <- false
	| false	-> shield <- true
  method die =
	print_endline "Emergency Temporal Shift!"
end
