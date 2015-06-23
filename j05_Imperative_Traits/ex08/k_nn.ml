(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   k_nn.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/22 15:36:40 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/23 18:50:29 by ngoguey          ###   ########.fr       *)
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
	((av -. bv) ** 2. +. acc, i + 1)
  in
  let (squaredsum, _) = Array.fold_left square (0., 0) a in
  sqrt squaredsum

let rad_eu_list (a, _) (b, _q) =
  eu_dist a b

let main_occurence ar k =
  let rec helper2 i v count =
	if i = k then
	  count
	else
	  begin
		let (cl', _) = ar.(i) in
		if v = cl' then
		  helper2 (i + 1) v (count + 1)
		else
		  helper2 (i + 1) v count
	  end
  in
  let rec helper i (n, cl) =
	if i = k then
	  cl
	else
	  begin
		let (cl', _) = ar.(i) in
		let occ = helper2 (i + 1) cl' 1 in
		if occ > n then
		  helper (i + 1) (occ, cl')
		else
		  helper (i + 1) (n, cl)
	  end	
  in
  helper 0 (0, "")
		 
let k_nn (l: radar list) k (r: radar) =
  let rec helper l l' =
	match l with
	| []								-> l'
	| (_, cl) as hd::tl
	  -> let diff = rad_eu_list r hd in
		 helper tl ((cl, diff)::l')
  in
  let l = helper l [] in
  let ar = Array.of_list l in
  let k = min (Array.length ar) k in
  Array.sort (fun (_, a) (_, b) -> int_of_float ((a -. b) *. 1000.)) ar;
  let ar = Array.sub ar 0 k in
  main_occurence ar k
				 

let () =
  let radars = [
	  ([|-0.67321; 0.80893; -0.40446; 0.06264; 0.97763; 0.04474; 0.95973|], "a");
	  ([|0.66667; -0.01366; 0.97404; 0.06831; 0.49590; 0.50137; 0.75683|], "b");
	  ([|0.66667; -0.01366; 0.97404; 0.06831; 0.49590; 0.50137; 0.75683|], "b");
	  ([|0.83609; 0.13215; 0.72171; 0.06059; 0.65829; 0.08315; 0.23888|], "c");
	  ([|0.12961; 0.43837; 0.20330; 0.49418; 0.12686; 0.44747; 0.13507|], "c");
	  ([|0.88085; 0.35232; 0.68389; 0.65128; 0.34816; 0.79784; 0.05832|], "b");
	  ([|0.90842; -0.29784; 0.86490; -0.62635; 0.69590; 0.79378; 0.29492|], "c");
	  ([|0.87111; 0.04326; 0.79946; 0.18297; 0.99009; 0.29292; 0.49590|], "a");
	  ([|0.86889; -0.07111; 1.0000; -0.02494; 1.0000; -0.06889; 0.1|], "y");
	] in
  print_string "Nearest class of test0 is (k = 4): ";
  print_endline (k_nn radars 4
					  ([|0.02337; -0.00592; -0.09924; -0.11949; -0.00763; -0.11824; 0.14706|], "test0"));
  print_string "Nearest radar of test0 is: ";
  print_endline (k_nn radars 1
						([|0.02337; -0.00592; -0.09924; -0.11949; -0.00763; -0.11824; 0.14706|], "test0"));
  print_string "Nearest class of test1 is (k = 4): ";
  print_endline (k_nn radars 4
					  ([|0.74852; -0.02811; 0.65680; -0.05178; 0.80621; 0.02811; 0.|], "test1"));
  print_string "Nearest radar of test1 is: ";
  print_endline (k_nn radars 1
						([|0.74852; -0.02811; 0.65680; -0.05178; 0.80621; 0.02811; 0.|], "test1"));
  print_string "Nearest class of test2 is (k = 4): ";
  print_endline (k_nn radars 4
					  ([|0.66667; -0.01366; 0.97404; 0.06831; 0.49590; 0.50137; 0.75683|], "test2"));
  print_string "Nearest radar of test2 is: ";
  print_endline (k_nn radars 1
						([|0.66667; -0.01366; 0.97404; 0.06831; 0.49590; 0.50137; 0.75683|], "test2"))


				
