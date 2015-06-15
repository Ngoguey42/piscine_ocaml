(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 10:19:45 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/15 10:32:31 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec loop s i =
  if i >= 0 then
	begin
	  print_char (String.get s i);
	  loop s (i - 1)
	end

let ft_print_rev s =
  loop s (String.length s - 1);
  print_char '\n'

let test s =
  Printf.printf "Test with [%s]: %!" s;
  ft_print_rev s

let () =
  test "";
  test "a";
  test "aA";
  test "abc";
  test "salut";
  test "lol"
