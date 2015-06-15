(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 10:00:36 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/15 14:58:48 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let is_valid i =
  let d3 = i / 100 in
  let d2 = i / 10 mod 10 in
  let d1 = i mod 10 in
  if d3 < d2 && d2 < d1 && d1 <> d2 && d2 <> d3 && d3 <> d1 then
	true
  else
	false

let printwid3 i =
  if i < 100 then
	print_string "0";
  print_int i

let print i comma =
  if comma then
	print_string ", ";
  printwid3 i

let rec loop i comma =
  if i <= 789 then
	begin
	  if is_valid i then
		print i comma;
	  loop (i + 1) true
	end

let ft_print_comb () =
  loop 012 false;
  print_string "\n"

let () =
  ft_print_comb ()
