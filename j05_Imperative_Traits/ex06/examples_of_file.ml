(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_file.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/22 13:09:26 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/22 13:16:16 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let parse_line line = 


let examples_of_files fname =
  let fchan = open_in fname in
  let l = ref [] in
  begin
	try
	  while true do
		let line = input_line fchan in
		l := !l @ [parse_line line]
	  done;
	with
	| End_of_file	-> ();
  end;
  l

let () =
  try
	begin
	  
	end
  with
  (* | Failure "no csv found :(" *)
	(* -> print_endline ("Catched : no csv found in file !!") *)
  | Failure "problem in csv format"
	-> print_endline ("Catched : csv file corrupted !!")
  | Invalid_argument m
	-> print_endline ("Catched \"" ^ m ^ "\" please give some arguments")
  | Sys_error m
	-> print_endline ("Catched \"" ^ m ^ "\" oh no !!")

					 (*
ocamlopt examples_of_file.ml && ./a.out
ocamlopt examples_of_file.ml && ./a.out ""
ocamlopt examples_of_file.ml && ./a.out "/exam"
ocamlopt examples_of_file.ml && ./a.out "lul"
ocamlopt examples_of_file.ml && ./a.out "."
ocamlopt examples_of_file.ml && ./a.out ionosphere.csv
	  let fname = Sys.argv.(1) in
					  *)
