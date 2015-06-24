(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   galifrey.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 17:30:45 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/24 18:49:26 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class galifrey =
object
  val dal = (new Army.army:Dalek.dalek Army.army)
  val doc = (new Army.army:Doctor.doctor Army.army)
  val peo = (new Army.army:People.people Army.army)
  method do_time_war () =
	Printf.printf "Olol j'ai le droit a printf !!!\n%!";
	Printf.printf "C'est genre comme une TIMEWAR\n%!";
end
