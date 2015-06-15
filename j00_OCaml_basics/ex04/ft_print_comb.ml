(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 10:00:36 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/15 10:18:29 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let nexta a b c =
  if b * 10 + c = 99 then
	a + 1
  else
	a

let nextb b c =
  if c = 9 then
	begin
	  if b = 9 then
		0
	  else
		b + 1
	end
  else
	b

let nextc c =
  if c = 9 then
	0
  else
	c + 1

let print a b c comma =
  if comma then
	print_string ", ";
  print_int a;
  print_int b;
  print_int c
			
let rec loop a b c comma =
  if a > 7 then
	()
  else
	begin
	  begin
		if a <> b && a <> c && c <> b && a < b && b < c then
		  print a b c comma
	  end;
	  loop (nexta a b c) (nextb b c) (nextc c) true
	end
	  
let ft_print_comb () =
  loop 0 1 2 false;
  print_string "\n"

let () =
  ft_print_comb ()
