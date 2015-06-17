(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sequence.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 17:47:55 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/17 18:33:19 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let sequence n =
  let rec revl src dst =
	match src with
	| []					-> dst
	| hd::tl				-> revl tl (hd::dst)
  in
  let gen src =
	let rec gen_helper src dst nb nbcount =
	  match src with
	  | [] when nb = -1		-> dst
	  | []					-> nb::nbcount::dst
	  | hd::tl when nb = -1	-> gen_helper tl dst				hd 1
	  | hd::tl when nb = hd	-> gen_helper tl dst				hd (nbcount + 1)
	  | hd::tl				-> gen_helper tl (nb::nbcount::dst)	hd 1
	in
	revl (gen_helper src [] (-1) 0) []
  in
  let rec loop n prev =
	if n = 0 then
	  prev
	else
	  loop (n - 1) (gen prev)
  in
  let rec string_of_int_list l s =
	match l with
	| []					-> s
	| hd::tl				-> string_of_int_list tl (s ^ string_of_int hd)
  in
  if n < 1 then
	""
  else
	string_of_int_list (loop (n - 1) [1]) ""


let test n =
  Printf.printf "Test with [%d]: %!" n;
  Printf.printf "\"%s\"\n%!" (sequence n)

let () =
  test (-1);
  test 0;
  test 1;
  test 2;
  test 3;
  test 4;
  test 5;
  test 6;
  test 7;
  test 8;
  test 9;
  test 10;
  
