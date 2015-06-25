(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/25 12:01:59 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/25 13:30:10 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  Printf.printf "\027[34m%s\027[0m\n%!" "Tests ex00:";
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.hydrogen)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.helium)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.carbon)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.nitrogen)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.oxygen)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.fluorine)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.sodium)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.magnesium)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.aluminum)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.silicon)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.phosphorus)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.sulfur)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.chlorine)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.argon)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.potassium)#to_string;
  Printf.printf "New Atom:%! to_string: %s\n%!" (new Atom.calcium)#to_string;
  Printf.printf "\027[34m%s\027[0m\n%!" "Tests ex01:";
  Printf.printf "New Molecule:%! to_string: %s\n%!" (new Molecule.water)#to_string;
  Printf.printf "New Molecule:%! to_string: %s\n%!" (new Molecule.carbon_dioxyde)#to_string;
  
  
