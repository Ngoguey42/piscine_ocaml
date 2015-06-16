(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 09:13:54 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/16 09:46:05 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_x n =

  let rec loop s n =
	if n <= 0 then
	  s
	else
	  loop (s ^ "x") (n - 1)
  in

  if n < 0 then
	"Error"
  else
	loop "" n

		 
let test n =
  Printf.printf "Test with [%+2d]: %s\n%!" n (repeat_x n)

let () =
  test (-1);
  test 0;
  test 1;
  test 2;
  test 3;
