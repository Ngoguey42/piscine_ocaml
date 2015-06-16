(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ackermann.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 09:49:09 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/16 10:06:25 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ackermann m n =
  
  let rec loop m n =
	if m = 0 then
	  n + 1
	else if n = 0 && m > 0 then
	  loop (m - 1) 1
	else	(* if m > 0 && n > 0 *)
	  loop (m - 1) (loop m (n - 1))
  in

  if m < 0 || n < 0 then
	-1
  else
	loop m n
  
let test m n =
  Printf.printf "Test with [m=%+2d, m=%+2d] = %!" m n;
  Printf.printf "%+d\n%!" (ackermann m n)

let () =
  test (-1) 7;
  test 7 (-1);
  test 0 0;
  test 0 1;
  test 0 2;
  test 0 3;
  test 0 4;
  test 0 5;
  test 0 6;
  test 0 7;
  test 1 0;
  test 1 1;
  test 1 2;
  test 1 3;
  test 1 4;
  test 1 5;
  test 1 6;
  test 1 7;
  test 2 0;
  test 2 1;
  test 2 2;
  test 2 3;
  test 2 4;
  test 2 5;
  test 2 6;
  test 2 7;
  test 3 0;
  test 3 1;
  test 3 2;
  test 3 3;
  test 3 4;
  test 3 5;
  test 3 6;
  test 3 7;
  test 4 0;
  test 4 1;
