(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 17:39:16 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 18:28:55 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let test msg l =
  Printf.printf "\027[35m%s:\027[0m\n%!" msg;
  Set.foreach l (fun str -> print_string ("\"" ^ str ^ "\"; "));
  Printf.printf "\n%!"
  
let test2 msg b =
  Printf.printf "\027[35m%s:\027[0m\n%!" msg;
  Printf.printf "%B\n%!" b
  

let () =
  let l1 = ["haha"; "hehe"; "hihi"; "hoho"; "huhu"; "tete"] in
  let l2 = ["hihi"; "hoho"; "huhu"; "tata"; "tete"; "titi"; "toto"; "tutu"] in
  let bindl1 = Set.bind l1 (fun str -> Set.return (str ^ "ZZ")) in
  let union12 = Set.union l1 l2 in
  let inter12 = Set.inter l1 l2 in
  let diff12 = Set.diff l1 l2 in
  let filter1 = Set.filter l1 (fun str -> String.contains str 'e') in
  let for_all1 = Set.for_all l1 (fun str -> String.length str = 4) in
  let exists1 = Set.exists l1 (fun str -> str = "titi") in
  test "l1:" l1;
  test "l2:" l2;
  test "Set.bind l1 (fun str -> Set.return (str ^ \"ZZ\"))" bindl1;
  test "Set.union l1 l2" union12;
  test "Set.inter l1 l2" inter12;
  test "Set.diff l1 l2" diff12;
  test "Set.filter l1 (fun str -> String.contains str 'e')" filter1;
  test2 "Set.for_all l1 (fun str -> String.length str = 4)" for_all1;
  test2 "Set.exists l1 (fun str -> str = \"titi\")" exists1;
  Printf.printf "\n%!"
  
