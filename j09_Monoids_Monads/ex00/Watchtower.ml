(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Watchtower.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 13:28:18 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 16:51:34 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type hour = int
let zero = 12
let add (a: hour) (b: hour) =
  let m =  (a + b) mod zero in
  if m > 0 then
	m
  else
	zero + m
let sub (a: hour) (b: hour) =
  let m =  (a - b) mod zero in
  if m > 0 then
	m
  else
	zero + m
