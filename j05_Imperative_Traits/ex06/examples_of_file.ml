(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_file.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/22 13:09:26 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/23 17:08:28 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let is_float_char = function
  | "-" | "+" | "." | "0" | "1" | "2" | "3"
  | "4" | "5" | "6" | "7" | "8" | "9"	-> true
  | _ 									-> false

(* Ugliest code i've ever written, for real *)
let parse_line fchan =
  let l = ref [] in
  let nbrstr = ref "" in
  let classstr = ref "" in
  begin
	try
	  while true do
		let c = really_input_string fchan 1 in
		if c = "\n" then
		  raise (Failure "EOL")
		else if c = " " then
		  ()
		else if c = "," then
		  begin
			l := !l @ [float_of_string !nbrstr];
			nbrstr := ""
		  end
		else if is_float_char c then
		  nbrstr := !nbrstr ^ c
		else
		  classstr := !classstr ^ c
	  done
	with Failure "EOL"	-> ()
  end;
  (Array.of_list !l, !classstr)
	
let examples_of_file fname =
  try
	let fchan = open_in fname in
	let l = ref [] in
	begin
	  try
		while true do
		  l := !l @ [parse_line fchan]
		done;
	  with
	  | End_of_file	-> ();
	end;
	!l
  with
  | Failure "float_of_string"
	-> print_endline ("Catched \"float_of_string\" csv file corrupted !!"); []
  | Invalid_argument m
	-> print_endline ("Catched \"" ^ m ^ "\" please give some arguments"); []
  | Sys_error m
	-> print_endline ("Catched \"" ^ m ^ "\" oh no !!"); []

let test fname =
  Printf.printf "Test with %s:\n%!" fname;
  let l = examples_of_file fname in
  let n = List.length l in
  Printf.printf "List length: %d\n%!" n;
  for i = 0 to List.length l - 1 do
	let (fl, cl) as l = List.nth l i in
	Printf.printf "Line %3d, class %s, numfloats %d\n%!" i cl (Array.length fl);
	
  done;
  Printf.printf "\n%!"
  

  
  
  let () =
	test "ionosphere.test.csv";
	test "ionosphere.train.csv";

	(*
ocamlopt examples_of_file.ml && ./a.out
ocamlopt examples_of_file.ml && ./a.out ""
ocamlopt examples_of_file.ml && ./a.out "/exam"
ocamlopt examples_of_file.ml && ./a.out "lul"
ocamlopt examples_of_file.ml && ./a.out "."
ocamlopt examples_of_file.ml && ./a.out ionosphere.csv
	  let fname = Sys.argv.(1) in
	 *)
