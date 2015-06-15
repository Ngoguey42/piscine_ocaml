(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_string_all.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 10:34:29 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/15 11:04:11 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec loop f s i =
  if i >= 0 then
	begin
	  if f (String.get s i) then
		loop (f) s (i - 1)
	  else
		false
	end
  else
	true

let ft_string_all f s =
  loop (f) s (String.length s - 1)

let is_lower c =
  c >= 'a' && c <= 'z'
let is_upper c =
  c >= 'A' && c <= 'Z'
let is_digit c =
  c >= '0' && c <= '9'

let test f fname s =
  Printf.printf "Test with ['%s' * \"%s\"]: %B\n" fname s (ft_string_all f s)

let () =
  test is_lower "is_lower" "";
  test is_lower "is_lower" "a";
  test is_lower "is_lower" "ab";
  test is_lower "is_lower" "12";
  test is_lower "is_lower" "YZ";
  test is_lower "is_lower" "ab1";
  test is_lower "is_lower" "abcAabc";
  test is_digit "is_digit" "";
  test is_digit "is_digit" "a";
  test is_digit "is_digit" "ab";
  test is_digit "is_digit" "12";
  test is_digit "is_digit" "YZ";
  test is_digit "is_digit" "ab1";
  test is_digit "is_digit" "abcAabc";
  test is_upper "is_upper" "";
  test is_upper "is_upper" "a";
  test is_upper "is_upper" "ab";
  test is_upper "is_upper" "12";
  test is_upper "is_upper" "YZ";
  test is_upper "is_upper" "ab1";
  test is_upper "is_upper" "abcAabc"



				
