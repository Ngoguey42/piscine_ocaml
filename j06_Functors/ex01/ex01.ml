(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 12:39:34 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/24 19:28:54 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let djb2 s =
  let len = String.length s in
  let rec helper i acc =
	if i = len then
	  acc
	else
	  helper (i + 1) (int_of_char (String.get s i) + (acc lsl 5) + acc)
  in
  helper 0 5381

module StringHashtbl =
  Hashtbl.Make(
	  struct
		type t = string
		let equal = (=)
		let hash = djb2
	  end)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
