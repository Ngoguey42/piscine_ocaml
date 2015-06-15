(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_test_sign.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 09:04:29 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/15 09:15:45 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_test_sign i =
  if i < 0 then
	print_endline "negative"
  else
	print_endline "positive"


let test_fun i =
  Printf.printf "Test with [%+03d]: " i;
  ft_test_sign i

let () =
  test_fun (-42);
  test_fun (-1);
  test_fun 0;
  test_fun 1;
  test_fun 42
