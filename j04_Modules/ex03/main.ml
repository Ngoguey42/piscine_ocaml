(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 18:02:36 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/19 18:07:47 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let test d =
  Printf.printf "Deck: %!";
  List.iter print_string (Deck.toStringList d);
  Printf.printf "\n%!";
  Printf.printf "Deck: %!";
  List.iter print_string (Deck.toStringListVerbose d);
  Printf.printf "\n%!";
  let (cur, d) = Deck.drawCard d in
  Printf.printf "Picked a %s %!\n" (Deck.Card.toStringVerbose cur);
  let (cur, d) = Deck.drawCard d in
  Printf.printf "Picked a %s %!\n" (Deck.Card.toStringVerbose cur);
  let (cur, d) = Deck.drawCard d in
  Printf.printf "Picked a %s %!\n" (Deck.Card.toStringVerbose cur);
  let (cur, d) = Deck.drawCard d in
  Printf.printf "Picked a %s %!\n" (Deck.Card.toStringVerbose cur);  
  Printf.printf "\n%!\n"
				  
let () =
  test (Deck.newDeck ())
	   
