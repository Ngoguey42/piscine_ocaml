(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   iter.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 11:53:55 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/16 12:06:25 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let iter f x n =
  let rec loop n acc =
	if n = 0 then
	  acc
	else
	  loop (n - 1) (f acc)
  in
  if n < 0 then
	-1
  else
	loop n x

let test f fname x n =
  Printf.printf "Test with [%s %d %d] = %d\n%!" fname x n (iter f x n)
  
let test' x n =
  test (fun x -> x * x) "(fun x -> x * x)" x n;
  test (fun x -> x * 2) "(fun x -> x * 2)" x n
		 
let () =
  test' 0 0;
  test' 0 1;
  test' 0 2;
  test' 1 0;
  test' 1 1;
  test' 1 2;
  test' 2 0;
  test' 2 1;
  test' 2 2;
  test' 2 3;
  test' 2 4;
  
