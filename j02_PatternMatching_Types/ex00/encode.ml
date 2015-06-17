(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 13:57:49 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/17 15:15:36 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let encode la =
  let rec loop la lb =	
  	match la, lb with
  	| [], _					-> lb
  	| hda::tla, []			-> loop tla [(1, hda)]
  	| hda::tla, hdb::tlb	-> savehead hda tla hdb tlb
  and savehead hda tla hdb tlb =
  	match hdb with
  	| (n, v) when v = hda	-> loop tla ((n + 1, v)::tlb)
  	| _						-> loop tla ((1, hda)::hdb::tlb)
  in
  let rec rev l l' =
	match l with
	| []					-> l'
	| hdl::tll				-> rev tll (hdl::l')
  in
  rev (loop la []) []

let print_f x =
  Printf.printf "%-10.2f%!" x
let print_d x =
  Printf.printf "%-10d%!" x
let print_s x =
  Printf.printf "%-10s%!" x
				
let printtup_f (n, x) =
  Printf.printf "%-10s%!" (Printf.sprintf "%.2f(%d)" x n)
let printtup_d (n, x) =
  Printf.printf "%-10s%!" (Printf.sprintf "%d(%d)" x n)
let printtup_s (n, x) =
  Printf.printf "%-10s%!" (Printf.sprintf "\"%s\"(%d)" x n)
  
let test fl fel l =
  Printf.printf "  List: %!";
  List.iter fl l;
  Printf.printf "\nResult: %!";
  List.iter fel (encode l);
  Printf.printf "\n\n%!"
								
let () =
  Printf.printf "FLOAT\n%!";
  test print_f printtup_f [];
  test print_f printtup_f [5.];
  test print_f printtup_f [5.; 5.];
  test print_f printtup_f [5.; 42.];
  test print_f printtup_f [5.; 42.; 5.];
  test print_f printtup_f [5.; 5.; 42.];
  test print_f printtup_f [5.; 5.; 42.; 5.; 5.; 5.; 42.];
  Printf.printf "INT\n%!";
  test print_d printtup_d [];
  test print_d printtup_d [5];
  test print_d printtup_d [5; 5];
  test print_d printtup_d [5; 42];
  test print_d printtup_d [5; 42; 5];
  test print_d printtup_d [5; 5; 42];
  test print_d printtup_d [5; 5; 42; 5; 5; 5; 42];
  Printf.printf "STRING\n%!";
  test print_s printtup_s [""];
  test print_s printtup_s ["5"];
  test print_s printtup_s ["5"; "5"];
  test print_s printtup_s ["5"; "42"];
  test print_s printtup_s ["5"; "42"; "5"];
  test print_s printtup_s ["5"; "5"; "42"];
  test print_s printtup_s ["5"; "5"; "42"; "5"; "5"; "5"; "42"];
