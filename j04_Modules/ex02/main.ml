(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 16:25:19 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/19 16:46:14 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

open Card
open Printf

let printcard c =
  Printf.printf "%s %!" (Card.toString c)

let printcard' c =
  Printf.printf "%s %!" (Card.toStringVerbose c)

				
let test l =
  Printf.printf "Showing card list: \n%!";
  List.iter	printcard l; Printf.printf "\n%!";
  List.iter	printcard' l; Printf.printf "\n%!";
  Printf.printf "Best: %s\n%!" (Card.toStringVerbose (Card.best l));
  Printf.printf "Checking the first one:\n%!";
  let c = List.nth l 0 in
  Printf.printf "isOf Card.Color.Club = %B\n%!" (Card.isOf c Card.Color.Club);
  Printf.printf "isSpade Card.Color.Club = %B\n%!" (Card.isSpade c);
  Printf.printf "isHeart Card.Color.Club = %B\n%!" (Card.isHeart c);
  Printf.printf "isDiamond Card.Color.Club = %B\n%!" (Card.isDiamond c);
  Printf.printf "isClub Card.Color.Club = %B\n%!" (Card.isClub c);
  Printf.printf "Comparing the two first\n%!";
  let c' = List.nth l 1 in
  let cmp = compare c c' in
  let mx = max c c' in
  let mn = min c c' in
  Printf.printf "compare:%d  max:%s  min:%s\n%!"
				(cmp)
				(Card.toStringVerbose mx)
				(Card.toStringVerbose mn);
  Printf.printf "\n%!"
				

let () =
  test (allSpades);
  test (allHearts);
  test (allDiamonds);
  test (allClubs);
  test ([
		   Card.newCard Card.Value.Jack Card.Color.Spade;
		   newCard Card.Value.Jack Card.Color.Heart;
		   newCard Card.Value.T6 Card.Color.Spade;
		   newCard Card.Value.T7 Card.Color.Club;
		   newCard Card.Value.Queen Card.Color.Diamond
		 ]);
