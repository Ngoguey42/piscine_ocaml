(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   crossover.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 15:18:45 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/18 19:36:43 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let crossover la lb =
  let rec is_in l v =
	match l with
	| []					-> false
	| hd::_ when hd = v		-> true
	| _::tl					-> is_in tl v
  in
  let rec loop l la lb =
	match la with
	| []											-> l
	| hd::tl when is_in lb hd						-> loop (hd::l) tl lb
	| _::tl											-> loop l tl lb
  in
  loop [] la lb

let print_f x =
  Printf.printf "%-10.2f%!" x
let print_d x =
  Printf.printf "%-4d%!" x
let print_s x =
  Printf.printf "%-10s%!" x
				
let test fprint la lb =
  Printf.printf "   la: %!";
  List.iter fprint la;
  Printf.printf "\n   lb: %!";
  List.iter fprint lb;
  Printf.printf "\nResul: %!";
  List.iter fprint (crossover la lb);
  Printf.printf "\n\n%!"

let () =
  test print_d [] [];
  test print_d [42] [];
  test print_d [] [42];
  test print_d [42] [42];
  test print_d [42] [99];
  test print_d [42] [99; 99];
  test print_d [42; 42] [99; 99; 99];
  test print_d [42; 99] [99; 42];
  test print_d [42; 99; 42; 101] [99; 42; 99; 103];
	
