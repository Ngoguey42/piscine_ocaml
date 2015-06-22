(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_file.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/22 13:09:26 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/22 15:19:54 by ngoguey          ###   ########.fr       *)
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
		 if closestdiff < 0. || diff < closestdiff then
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
  let cl = one_nn l r in
  Printf.printf "radar class: \"%s\" \n%!" cl;
  Printf.printf "\n%!"

let () =
  let r1 = ([|1.;0.;0.84710;0.13533;0.73638;-.0.06151;
			  0.87873;0.08260;0.88928;-.0.09139;0.78735;
			  0.06678;0.80668;-.0.00351;0.79262;-.0.01054;
			  0.85764;-.0.04569;0.87170;-.0.03515;0.81722;
			  -.0.09490;0.71002;0.04394;0.86467;-.0.15114
			  ;0.81147;-.0.04822;0.78207;-.0.00703;0.75747;
			  -.0.06678;0.85764;-.0.06151|], "lol")
  in
  let r2 = ([|1.;0.;0.94331;0.19959;0.96132;
			  0.40803;0.80514;0.56569;0.56687;
			  0.70830;0.41836;0.83230;0.14939;
			  0.89489;0.05167;0.93682;-.0.24742;
			  0.83939;-.0.42811;0.75554;-.0.50251;
			  0.62563;-.0.65515;0.50428;-.0.68851;
			  0.30912;-.0.77097;0.15619;-.0.75406;
			  -.0.04399;-.0.75199;-.0.17921;-.0.66932;
			  -.0.34367|], "lol2")
  in
  let r3 = ([|1.;1263150.;0.94331;0.19959;0.96132;
			  0.40803;0.80514;0.56569;0.56687;
			  0.70830;0.41836;0.83230;0.14939;
			  0.89489;0.05167;0.93682;-.0.24742;
			  0.83939;-.0.42811;0.75554;-.0.50251;
			  0.62563;-.0.65515;0.50428;-.0.68851;
			  0.30912;-.0.77097;0.15619;-.0.75406;
			  -.0.04399;-.0.75199;-.0.17921;-.0.66932;
			  -.0.34367|], "lol2")
  in
  test "ionosphere.test.csv" r1;
  test "ionosphere.test.csv" r2;
  test "ionosphere.test.csv" r3;
  test "ionosphere.train.csv" r1;
  test "ionosphere.train.csv" r2;
