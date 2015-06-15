(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_power.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 09:37:46 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/15 09:51:31 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec loop a b acc =
  if b <= 0 then
	acc
  else
	loop a (b - 1) (acc * a)

let ft_power a b =
  loop a b 1

let test a b =
  Printf.printf "Test with [%d^%d] = %d\n" a b (ft_power a b)
				
let () =
  test 0 1;
  test 0 2;
  test 0 3;
  test 1 0;
  test 1 1;
  test 1 2;
  test 1 3;
  test 2 0;
  test 2 1;
  test 2 2;
  test 2 3;
  test 3 0;
  test 3 1;
  test 3 2;
  test 3 3;
  test 4 0;
  test 4 1;
  test 4 2;
  test 4 3
