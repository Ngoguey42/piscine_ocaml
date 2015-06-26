(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Try.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 15:41:32 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 16:25:35 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a t = Success of 'a | Failure of exn

let return v =
  Success v

let bind vt f =
  (* V MUST BE A SUCCESS (SUBJECT) *)
  try
	match vt with
	| Success v					-> f v
	| _							-> failwith "V must be a success"
  with e -> Failure e

let recover (vt: 'a) (f: exn -> 'b t)  =
  (* V MUST BE A FAILURE (SUBJECT) *)
  match vt with
  | Failure e					-> f e
  | _							-> vt

exception Salut
								 
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
