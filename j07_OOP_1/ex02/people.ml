(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   people.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 13:54:15 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/24 15:13:46 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class people =
object
  initializer print_endline "people CTOR"
  val name = "default"
  val hp = 100
  method to_string = name ^ ": " ^ (string_of_int hp) ^ "hp"
  method talk = print_endline ("I'm " ^ name ^ "! Do you know the Doctor?")
  method die = print_endline "Aaaarghh!"
end


  
