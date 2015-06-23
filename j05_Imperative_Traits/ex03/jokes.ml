(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 19:02:47 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/23 19:03:20 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let parse_file fchan =
  let l = ref [] in
  begin
	try
	  while true do
		let line = input_line fchan in
		l := line::(!l);
	  done;
	with
	| End_of_file	-> ();
  end;
  Array.of_list !l

let () =
  try
	begin
	  let fname = Sys.argv.(1) in
	  let fchan = open_in fname in
	  let jokes = parse_file fchan in
	  let n = Array.length jokes in
	  if n <= 0 then
		failwith "no joke found :(";
	  Random.self_init ();
	  print_endline (jokes.(Random.int n))
	end
  with
  | Failure "no joke found :("
	-> print_endline ("Catched : no joke found in file !!")
  | Invalid_argument m
	-> print_endline ("Catched \"" ^ m ^ "\" please give some arguments")
  | Sys_error m
	-> print_endline ("Catched \"" ^ m ^ "\" oh no !!")

					 (*
ocamlopt jokes.ml && ./a.out
ocamlopt jokes.ml && ./a.out ""
ocamlopt jokes.ml && ./a.out "/exam"
ocamlopt jokes.ml && ./a.out "lul"
ocamlopt jokes.ml && ./a.out "."
ocamlopt jokes.ml && ./a.out empty.joke
ocamlopt jokes.ml && ./a.out full.joke
ocamlopt jokes.ml && ./a.out one.joke
					  *)
