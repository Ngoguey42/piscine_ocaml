(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/25 12:01:59 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/25 18:12:04 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  Printf.printf "\027[34m%s\027[0m\n%!" "Tests ex02:";
  Printf.printf "New Molecule: %s\n" (new Alkane.methane)#to_string;
  Printf.printf "New Molecule: %s\n" (new Alkane.ethane)#to_string;
  Printf.printf "New Molecule: %s\n" (new Alkane.propane)#to_string;
  Printf.printf "New Molecule: %s\n" (new Alkane.heptane)#to_string;
  Printf.printf "New Molecule: %s\n" (new Alkane.octane)#to_string;
  Printf.printf "\027[34m%s\027[0m\n%!" "Tests ex04:";
  let a1 = new Alkane.methane in
  let a2 = new Alkane.ethane in
  let r1 = new Alkane.alkane_combustion [a1] in
  let r2 = new Alkane.alkane_combustion [a2] in
  let r3 = new Alkane.alkane_combustion [a2; a1] in
  let r4 = new Alkane.alkane_combustion [a2; a2; a2; a1] in
  Printf.printf "\027[36mNew reaction:\027[0m \n%s\nBalancing:\n%s\n"
				r1#to_string (r1#balance)#to_string;
  Printf.printf "\027[36mNew reaction:\027[0m \n%s\nBalancing:\n%s\n"
				r2#to_string (r2#balance)#to_string;
  Printf.printf "\027[36mNew reaction:\027[0m \n%s\nBalancing:\n%s\n"
				r3#to_string (r3#balance)#to_string;
  Printf.printf "\027[36mNew reaction:\027[0m \n%s\nBalancing:\n%s\n"
				r4#to_string (r4#balance)#to_string;
