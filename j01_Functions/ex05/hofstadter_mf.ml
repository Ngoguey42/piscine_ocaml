(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   hofstadter_mf.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 11:39:10 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/16 11:52:03 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec hfs_f n =
  let rec loop n =
	if n = 0 then
	  1
	else
	  n - hfs_m (loop (n - 1))
  in
  if n < 0 then
	-1
  else
	loop n

and hfs_m n =
  let rec loop n =
	if n = 0 then
	  0
	else
	  n - hfs_f (loop (n - 1))
  in
  if n < 0 then
	-1
  else
	loop n

let test n =
  Printf.printf "Test with [%s * %d] = %!" "hfs_f" n;
  Printf.printf "%d\n%!" (hfs_f n);
  Printf.printf "Test with [%s * %d] = %!" "hfs_m" n;
  Printf.printf "%d\n%!" (hfs_m n)

let () =
  test  (-1);
  test  0;
  test  1;
  test  2;
  test  3;
  test  4;
  test  5;
  test  6;
  test  7;
  test  8;
  test  9;
  test  10;
  test  11;
  test  12;
  test  13;
  test  14;
  
