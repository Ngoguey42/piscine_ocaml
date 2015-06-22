(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   eu_dist.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/22 12:21:03 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/22 12:46:24 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let eu_dist a b =
  let n = Array.length a in
  if n <> Array.length b then
	failwith "Arrays have different lengths";
  if n = 0 then
	failwith "Arrays are empty !!";
  (* let square i av = *)
  (* 	let bv = b.(i) in *)
  (* 	(av - bv) ** 2. *)
  (* in *)
  (* let squaredarray = Array.mapi square a in *)
  let square (acc, i) av =
	let bv = b.(i) in
	((av -. bv) ** 2., i + 1)
  in
  let (squaredsum, _) = Array.fold_left square (0., 0) a in
  sqrt squaredsum


let test a b =
  Printf.printf "Test with: \n%!";
  Printf.printf "a: [|%!";
  Array.iter (fun v -> print_string ((string_of_float v) ^ "; ")) a;
  Printf.printf "|]\nb: [|%!";
  Array.iter (fun v -> print_string ((string_of_float v) ^ "; ")) b;
  Printf.printf "|]\n";
  Printf.printf "eu_dist a b = %!";
  try
	Printf.printf "%.f%!\n" (eu_dist a b)
  with
  | Failure m
	->	Printf.printf "Catched \"%s\" :(\n%!" m
					  

let () =
  test [||] [||];
  test [|1.|] [||];
  test [|1.|] [|1.|];
  test [|1.; 1.|] [|1.; 1.|];
  test [|42.|] [|-.42.|];
  test [|-42.|] [|42.|];
  test [|45.; 98456.; 9856.; 84.; 8946.; 89.; 8945.|]
	   [|5.; 9856.; 986.; 4.; 846.; 8.; 895.|];
					  
