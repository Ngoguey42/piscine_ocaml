(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 12:39:34 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/23 13:41:55 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module StringHashtbl = Hashtbl.Make(
						   struct
							 type t = string
							 let equal = (=)
							 let hash = Hashtbl.seeded_hash 42
						   end)


let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
