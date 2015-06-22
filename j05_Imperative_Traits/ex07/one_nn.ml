(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_file.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/22 13:09:26 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/22 14:50:35 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type radar = float array * string

let eu_dist a b =
  let n = Array.length a in
  if n <> Array.length b then
	failwith "Arrays have different lengths";
  if n = 0 then
	failwith "Arrays are empty !!";
  let square (acc, i) av =
	let bv = b.(i) in
	((av -. bv) ** 2., i + 1)
  in
  let (squaredsum, _) = Array.fold_left square (0., 0) a in
  sqrt squaredsum

let rad_eu_list (a, _) (b, _q) =
  eu_dist a b
		  
let one_nn (l: radar list) (r: radar) =
  let rec helper l closestdiff closest =
	match l with
	| []								-> (closestdiff, closest)
	| hd::tl
	  -> let diff = rad_eu_list r hd in
		 if diff < 0. || diff < closestdiff then
		   helper tl diff hd
		 else
		   helper tl closestdiff closest
  in
  let (diff, closest) = helper l (-.1.) ([||], "") in
  let (_, cl) = closest in
  cl
	

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
	
let examples_of_files fname =
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

let test fname r =
  Printf.printf "Test with %s:\n%!" fname;
  let l = examples_of_files fname in
  let n = List.length l in
  Printf.printf "List length: %d\n%!" n;
  Printf.printf "\n%!"
				

				
				
let () =
  test "ionosphere.test.csv";
  test "ionosphere.train.csv";

