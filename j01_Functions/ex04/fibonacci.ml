(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   test.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 11:14:17 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/16 11:14:18 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let fibonacci n =
  let rec loop n acc =
	if n < 0 then
	  -1
	else if n <= 1 then
	  n + acc
	else
	  loop (n - 2) (loop (n - 1) acc)
  in
  loop n 0

let fib = fibonacci


let rec fib2 n =
  if n = 0 then
	0
  else if n = 1 then
	1
  else 
	(fib2 (n - 2)) + (fib2 (n - 1))

let fib3 n =
  let rec loop a b n =
	if n > 0 then
	  loop b (a + b) (n - 1)
	else
	  a
  in
  loop 0 1 n

let test n = 
  Printf.printf "Test with [%02d] = %!" n;
  Printf.printf "%d\n%!" (fib n)
				
let () =
  test 44;
  test 90;
