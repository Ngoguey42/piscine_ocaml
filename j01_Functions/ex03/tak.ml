(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tak.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 10:07:52 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/16 10:18:43 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let tak x y z =

  let rec loop x y z =
	if y >= x then
	  z
	else
	  loop (loop (x - 1) y z) (loop (y - 1) z x) (loop (z - 1) x y)
  in

  loop x y z

let test x y z =
  Printf.printf "Test with [%+2d %+2d %+2d] = %!" x y z;
  Printf.printf "%d\n%!" (tak x y z)

let min = 0
let max = 16
		
let testz x y =
  for i = min to max do
	test x y i
  done

let testy x =
  for i = min to max do
	testz x i
  done

	
let () =
  for i = min to max do
	testy i
  done;
  test 1 2 3;
  test 5 23 7;
  test 9 1 0;
  test 1 1 1;
  test 0 42 0;
  test 23498 98734 98776;
  
