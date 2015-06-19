(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Color.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 14:22:26 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/19 14:29:22 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type t = Spade | Heart | Diamond | Club

let all = [Spade; Heart; Diamond; Club]
			
let toString (c: t) =
  let string_of_t = function
	| Spade				-> "S"
	| Heart				-> "H"
	| Diamond			-> "D"
	| Club				-> "C"
  in
  string_of_t c

let toStringVerbose (c: t) =
  let string_of_t = function
	| Spade				-> "Spade"
	| Heart				-> "Heart"
	| Diamond			-> "Diamond"
	| Club				-> "Club"
  in
  string_of_t c
			  
