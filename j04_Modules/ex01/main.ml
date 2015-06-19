(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 15:12:25 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/19 15:12:25 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

open Value
open List

let test i =
  try begin
	  let c = List.nth Value.all i in
	  let s = toInt c in
	  let s1 = toString c in
	  let s2 = toStringVerbose c in
	  Printf.printf "Card: (%2d) \"%2s\" \"%5s\" next:%!" s s1 s2;
	  let s3 = toString (next c) in
	  Printf.printf "%2s prev:%!" s3;
	  let s4 = toString (prev c) in
	  Printf.printf "%2s\n%!" s4
	end
  with invalid_arg -> Printf.printf "OMG CATCH\n%!"
		
let () =
  Random.self_init ();
  test (Random.int (13) + 0);
  test (Random.int (13) + 0);
  test (Random.int (13) + 0);
  test (Random.int (13) + 0);
  test (Random.int (13) + 0);
  test (Random.int (13) + 0);
  test (Random.int (13) + 0);
  test (Random.int (13) + 0);
  test (Random.int (13) + 0);
  test (Random.int (13) + 0);
  test (Random.int (13) + 0);
  test (Random.int (13) + 0);
