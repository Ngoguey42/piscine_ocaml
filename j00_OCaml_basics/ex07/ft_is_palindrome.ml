(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_is_palindrome.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 11:27:13 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/16 17:37:58 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec loop s i j =
  if i < j then
	begin
	  if String.get s i <> String.get s j then
		false
	  else
		loop s (i + 1) (j - 1)
	end
  else
	true

let ft_is_palindrome s =
  loop s 0 (String.length s - 1)

let test s =
  Printf.printf "Test [%s] = %B\n%!" s (ft_is_palindrome s)

let () =
  test "";
  test "a";
  test "ab";
  test "aba";
  test "abab";
  test "ababa";
  test "aa";
  test "abba";
  test "abbab";
  test "abbaba";
  test "abbabba";
