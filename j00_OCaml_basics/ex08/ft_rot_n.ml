(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_rot_n.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 12:03:19 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/15 18:07:50 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let delta = int_of_char 'z' - int_of_char 'a' + 1

let rec looplower c =
  if c > int_of_char 'z' then
	looplower (c - delta)
  else
	c

let rec loopupper c =
  if c > int_of_char 'Z' then
	loopupper (c - delta)
  else
	c

let is_lower c =
  c >= 'a' && c <= 'z'

let is_upper c =
  c >= 'A' && c <= 'Z'

let rot i c =
  if is_lower c then
	char_of_int (looplower (int_of_char c + i))
  else if is_upper c then
	char_of_int (loopupper (int_of_char c + i))
  else
	c

let ft_rot_n i s =
  String.map (rot i) s

let test i s =
  Printf.printf "Test rot%-3d \"%s\"=>\"%s\"  \n%!" i s (ft_rot_n i s)

let () =
  test 0 "";
  test 0 "a";
  test 0 "ab";
  test 0 "YZ";
  test 0 "Az";
  test 1 "";
  test 1 "a";
  test 1 "ab";
  test 1 "YZ";
  test 1 "Az";
  test 26 "";
  test 26 "a";
  test 26 "ab";
  test 26 "YZ";
  test 26 "Az";
  test 42 "";
  test 42 "a";
  test 42 "ab";
  test 42 "YZ";
  test 42 "Az";
