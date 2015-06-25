(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/25 12:01:59 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/25 14:51:22 by ngoguey          ###   ########.fr       *)
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
  Printf.printf "Compare Al Al last: %B\n%!" ((new Atom.aluminum)#equals (new Atom.aluminum));
  Printf.printf "Compare Al H last: %B\n%!" ((new Atom.aluminum)#equals (new Atom.hydrogen));
  Printf.printf "\027[34m%s\027[0m\n%!" "Tests ex01:";
  Printf.printf "New Molecule:%! to_string: %s\n%!" (new Molecule.water)#to_string;
  Printf.printf "New Molecule:%! to_string: %s\n%!" (new Molecule.carbon_monoxyde)#to_string;
  Printf.printf "New Molecule:%! to_string: %s\n%!" (new Molecule.carbon_monoxyde)#to_string;
  Printf.printf "New Molecule:%! to_string: %s\n%!" (new Molecule.aspirin)#to_string;
  Printf.printf "New Molecule:%! to_string: %s\n%!" (new Molecule.caffeine)#to_string;
  Printf.printf "Compare water water last: %B\n%!" ((new Molecule.water)#equals (new Molecule.water));
  Printf.printf "Compare water aspirin last: %B\n%!" ((new Molecule.water)#equals (new Molecule.aspirin));  
  Printf.printf "\027[34m%s\027[0m\n%!" "Tests ex02:";
  let n = 0 in
  Printf.printf "New Alkane %2d: %s\n" n (new Alkane.alkane n)#to_string;
  let n = 1 in
  Printf.printf "New Alkane %2d: %s\n" n (new Alkane.alkane n)#to_string;
  let n = 2 in
  Printf.printf "New Alkane %2d: %s\n" n (new Alkane.alkane n)#to_string;
  let n = 3 in
  Printf.printf "New Alkane %2d: %s\n" n (new Alkane.alkane n)#to_string;
  let n = 4 in
  Printf.printf "New Alkane %2d: %s\n" n (new Alkane.alkane n)#to_string;
  let n = 5 in
  Printf.printf "New Alkane %2d: %s\n" n (new Alkane.alkane n)#to_string;
  Printf.printf "New Molecule: %s\n" (new Alkane.methane)#to_string;
  Printf.printf "New Molecule: %s\n" (new Alkane.ethane)#to_string;
  Printf.printf "New Molecule: %s\n" (new Alkane.octane)#to_string;
  
