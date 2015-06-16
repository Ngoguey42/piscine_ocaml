(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   leibniz_pi.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 14:09:16 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/16 14:52:12 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let leibniz_pi dt =
(* Comparison between 'acc' and 'pi4' *)
  let pi4 = atan 1. in
  let compare dt' =
	if dt' <= dt then
	  true
	else
	  false
  in
  let found v =
	let dt' = 1. -. v /. pi4 in
	if dt' >= 0. then
	  compare dt'
	else
	  compare (-. dt')
  in
(* Sigma loop *)
  let formula i =
	(-1.) ** i /. (2. *. i +. 1.)
  in
  let rec loop i i' acc =
	if found acc then
		i
	else
		loop (i + 1) (i' +. 1.) (acc +. formula i')
  in
  
  if dt < 0. then
	-1
  else
	loop 0 0. 0.
		
let test dt =
  Printf.printf "Test with [%.15f aka %.13f%%] = %!" dt (dt *. 100.);
  Printf.printf "%d\n%!" (leibniz_pi dt)
  
let () =
  test (10.			/. 100.);
  test (1.			/. 100.);
  test (0.1			/. 100.);
  test (0.01		/. 100.);
  test (0.001		/. 100.);
  test (0.0001		/. 100.);
  test (0.00001		/. 100.);
  test (0.000001	/. 100.);
  test (0.0000001	/. 100.);
