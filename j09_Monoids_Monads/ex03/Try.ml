(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Try.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 15:41:32 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 17:33:38 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a t = Success of 'a | Failure of exn

let return v =
  Success v

exception Salut
		  
let bind vt f =
  try
	match vt with
	| Success v					-> f v
	| Failure e as truc			-> truc
  with e						-> Failure e

let recover vt f =
  match vt with
  | Failure e					-> f e
  | _							-> vt

let filter vt f =
  match vt with
  | Success v when f v			-> vt
  | Success v					-> Failure Salut
  | _							-> vt

let flatten (vtt: 'a t t) =
  match vtt with
  | Success (Success _ as vt)	-> vt
  | Success (Failure _ as vt)	-> vt
  | Failure _ as vt				-> vt

let print_t_int vt =
  print_string "\027[35m";
  begin
	match vt with
	| Success i			-> print_string ("Success(" ^ string_of_int i ^ ")")
	| Failure _			-> print_string "Failure(...)"
  end;
  print_endline "\027[0m"
