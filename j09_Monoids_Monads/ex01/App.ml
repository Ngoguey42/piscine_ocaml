(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   App.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 13:54:18 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 14:00:26 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type project = string * string * int

let zero: project = ("", "", 0)
	
let combine ((sa, _, ga): project) ((sb, _, gb):project): project =
  let sum = ga + gb in
  if sum >= 80 then
	(sa ^ sb, "succeed", 80)
  else
	(sa ^ sb, "failed", sum)

let fail ((sa, _, _): project): project =
  (sa, "failed", 0)

let succeed ((sa, _, _): project): project =
  (sa, "succeed", 80)
