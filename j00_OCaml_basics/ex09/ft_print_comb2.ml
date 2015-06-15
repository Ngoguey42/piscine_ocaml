(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb2.ml                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 14:12:25 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/15 14:46:38 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let printwid2 i =
  if i < 10 then
	print_char '0';
  print_int i

let print i j comma =
  if comma then
	begin
	  print_char ',';
	  print_char ' '
	end;
  printwid2 i;
  print_char ' ';
  printwid2 j

let rec loop i j comma =
  if i <= 98 then
	begin
	  if i < j && j <> i then
		print i j comma;
	  let j' = j + 1 in
	  loop (i + j' / 100) (j' mod 100) true
	end

let ft_print_comb2 () =
  loop 0 1 false;
  print_char '\n'

let () =
  ft_print_comb2 ()
