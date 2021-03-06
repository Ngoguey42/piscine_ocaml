(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 13:54:14 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/24 16:47:37 by ngoguey          ###   ########.fr       *)
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
  d1#use_sonic_screwdriver;
  d1#use_sonic_screwdriver;
  Printf.printf "Ma fonction regenerate fonctionne, jte le jure\n%!";
