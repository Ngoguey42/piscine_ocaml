(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Watchtower.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 13:28:18 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 13:45:08 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type hour = int
let zero = 12
let add (a: hour) (b: hour) =
  let r = a + b in
  if r >= 0 then
	r mod zero
  else
	(zero - (~-r mod zero)) mod zero
let sub (a: hour) (b: hour) =
  let r = a - b in
  if r >= 0 then
	r mod zero
  else
	(zero - (~-r mod zero)) mod zero
