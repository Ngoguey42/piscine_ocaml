(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   cipher.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 16:42:19 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/18 18:13:16 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rot42 s =
  Uncipher.rotglobal 42 s

let caesar i s =
  Uncipher.rotglobal i s

let xor i s =
  let helper i c =
	char_of_int (int_of_char c lxor i)
  in
  String.map (helper i) s
  
let ft_crypt s fl =
  let rec revl src dst =
	match src with
	| []-> dst
	| hd::tl-> revl tl (hd::dst)
  in
  Uncipher.ft_uncrypt s (revl fl [])

(* TESTS: *)
open Uncipher
					  
let test s fl1 fl2 =
  let s' = ft_crypt s fl1 in
  Printf.printf "Test with \"%s\" => \"%s\" => \"%s\"\n%!" s s'
				(ft_uncrypt s' fl2)
				
let () =
  test "" [] [];
  test "" [rot42] [unrot42];
  test "a" [rot42] [unrot42];
  test "a" [caesar 1] [uncaesar 1];
  test "a" [xor 10] [xor 10];
  test "z" [rot42] [unrot42];
  test "z" [caesar 1] [uncaesar 1];
  test "abcXYZ*-+/ABCxyz" [rot42] [unrot42];
  test "abcXYZ*-+/ABCxyz" [caesar 1] [uncaesar 1];
  test "abcXYZ*-+/ABCxyz" [xor 10] [xor 10];
  test "abcXYZ*-+/ABCxyz" [xor 10;rot42;caesar 42] [xor 10;unrot42;uncaesar 42];
  
