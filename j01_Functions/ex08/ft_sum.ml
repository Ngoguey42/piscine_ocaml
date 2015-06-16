(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 12:52:23 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/16 14:06:40 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_sum f x n =
  let rec loop i acc =
	if i > n then
	  acc
	else
	  loop (i + 1) (acc +. f i)
  in
  loop x 0.

let test f fname x n =
  Printf.printf "Test with [\027[35m%-19s\027[0m %d %d] = %f\n%!" fname x n (ft_sum f x n)

let test' x y =
  test (fun i -> float_of_int (i * i)) "(fun i -> float_of_int (i * i))" x y;
  test (fun i -> float_of_int i ** 3.14) "(fun i -> float_of_int i ** 3.14)" x y

let () =
  test' (-5) 5;
  test' 0 0;
  test' 0 1;
  test' 1 0;
  test' 0 2;
  test' 0 3;
  test' 0 4;
  test' 1 2;
  test' 1 3;
  test' 1 4;
  test' 2 2;
  test' 2 3;
  test' 2 4;
  
  
