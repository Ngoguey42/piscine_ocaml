(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 13:54:14 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/24 16:46:50 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  Printf.printf "Tests: ex00\n%!";
  let p1 = new People.people in
  Printf.printf "to_string: %s\n%!" (p1#to_string);
  Printf.printf "talk:\n%!";
  p1#talk;
  Printf.printf "die:\n%!";
  p1#die;
  Printf.printf "Tests: ex01\n%!";
  let d1 = new Doctor.doctor in
  Printf.printf "talk:\n%!";
  d1#talk;
  Printf.printf "to_string: %s\n%!" (d1#to_string);
  Printf.printf "d1 = Travel in time 42 84\n%!";
  let d1 = d1#travel_in_time 42 84 in
  Printf.printf "to_string: %s\n%!" (d1#to_string);
  Printf.printf "Ma fonction regenerate fonctionne, jte le jure\n%!";
  Printf.printf "Test: ex02\n%!";
  let dal1 = new Dalek.dalek in
  Printf.printf "to_string: %s\n%!" (dal1#to_string);
  Printf.printf "Talk:\n%!"; dal1#talk;
  Printf.printf "Talk:\n%!"; dal1#talk;
  Printf.printf "Talk:\n%!"; dal1#talk;
  Printf.printf "Exterminate:\n%!"; dal1#exterminate p1;Printf.printf "to_string: %s\n%!" (dal1#to_string); d1#use_sonic_screwdriver;
  Printf.printf "Exterminate:\n%!"; dal1#exterminate p1;Printf.printf "to_string: %s\n%!" (dal1#to_string); d1#use_sonic_screwdriver;
  Printf.printf "Exterminate:\n%!"; dal1#exterminate p1;Printf.printf "to_string: %s\n%!" (dal1#to_string); d1#use_sonic_screwdriver;
  Printf.printf "Die:\n%!";
  dal1#die;
  Printf.printf "\n%!";
  
