(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 13:55:58 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 14:08:04 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let print_proj ((str, status, grade): App.project) =
  print_endline ("\"" ^ str ^ "\""
				 ^ " " ^ status ^ " "
				 ^ " " ^ string_of_int grade ^ "/80") 
let test p0 p1 =
  Printf.printf "P0     : %!"; print_proj p0;
  Printf.printf "P1     : %!"; print_proj p1;
  Printf.printf "Combine: %!"; print_proj (App.combine p0 p1);
  Printf.printf "\n%!"
  
let () =
  let t0 = App.zero in
  test t0 t0;
  let t1 = App.fail ("Coucou", "", 0) in
  let t2 = App.succeed ("Coucou", "", 0) in
  test t1 t2;
  test t1 t1;
  test t2 t2;
  test ("Hel", "failed", 40) ("lo", "failed", 40);
  test ("Hel", "failed", 39) ("lo", "failed", 40);
  test ("Hel", "failed", 41) ("lo", "failed", 40);

  
