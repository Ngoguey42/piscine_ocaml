(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   uncipher.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 16:42:28 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/18 18:07:19 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rotglobal i s =
  let is_upper = function
	| 'A'..'Z'				-> true
	| _						-> false
  in
  let is_lower = function
	| 'a'..'z'				-> true
	| _						-> false
  in
  let dorot c i base =
	(* let sum = int_of_char c + i - base in *)
	let sum = (int_of_char c + i - base) mod 26 in
	(* Printf.printf "\n%!"; *)
	(* let sum' = sum mod 26 in *)
	(* print_char c; *)
	(* print_char ' '; *)
	(* print_char (char_of_int (sum mod 26 + base)); *)
	(* print_char ' '; *)
	(* print_int (sum); print_char ' '; *)
	(* print_int sum'; print_char ' '; *)
	(* print_int (sum' + base); print_char ' '; *)
	(* print_char '\n'; *)
	if sum < 0 then
	  char_of_int (26 + sum + base)
	else
	  char_of_int (sum + base)
  in
  let helper i c =
	if i <> 0 && is_lower c then
	  dorot c i (int_of_char 'a')
	else if i <> 0 && is_upper c then
	  dorot c i (int_of_char 'A')
	else
	  c
  in
  String.map (helper i) s

let unrot42 s =
  rotglobal (-42) s

let uncaesar i s =
  rotglobal (-i) s

let ft_uncrypt s fl =
  let rec helper s fl =
	match fl with
	| []					-> s
	| hd::tl				-> helper (hd s) tl
  in
  helper s fl
