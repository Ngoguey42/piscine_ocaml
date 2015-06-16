(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   test.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 11:14:17 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/16 11:38:00 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let fibonacci n =
  let rec loop a b n =
	if n > 0 then
	  loop b (a + b) (n - 1)
	else
	  a
  in
  if n < 0 then
	-1
  else
	loop 0 1 n

let test n = 
  Printf.printf "Test with [%02d] = %!" n;
  Printf.printf "%d\n%!" (fibonacci n)
				
let () =
  test (-1);
  test 0;
  test 1;
  test 2;
  test 3;
  test 4;
  test 5;
  test 6;
  test 7;
  test 8;
  test 9;
  
