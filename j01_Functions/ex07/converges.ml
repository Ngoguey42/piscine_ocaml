(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   converges.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/16 12:07:16 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/16 13:03:19 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let converges f x n =
  let rec loop n prev =
	if n = 0 then
	  false
	else
	  begin
		let prev' = f prev in
		if prev' = prev then
		  true
		else
		  loop (n - 1) prev'
	  end
  in
  if n < 0 then
	false
  else
	(* loop n x *)
	loop (n + 1) x

let test f fname x n =
  Printf.printf "Test with [\027[35m%-19s\027[0m %d %d] = %B\n%!" fname x n (converges f x n)

let test' x n =
  test (( * ) 2) "(( * ) 2)" x n;
  test (fun x -> x / 2) "(fun x -> x / 2)" x n

let testf f fname x n =
  Printf.printf "Test with [\027[35m%-19s\027[0m %f %d] = %B\n%!" fname x n (converges f x n)

let testf' x n =
  test (( * ) 2) "(( * ) 2)" x n;
  test (fun x -> x / 2) "(fun x -> x / 2)" x n

let () =
  test' 0 0;
  test' 1 0;
  test' 2 0;
  test' 3 0;
  test' 0 1;
  test' 1 1;
  test' 2 1;
  test' 3 1;
  test' 0 2;
  test' 1 2;
  test' 2 2;
  test' 3 2;
  test' 0 3;
  test' 1 3;
  test' 2 3;
  test' 3 3;
  test (( * ) 2) "(( * ) 2)" 2 5;
  test (fun x -> x / 2) "(fun x -> x / 2)" 2 3;
  test (fun x -> x / 2) "(fun x -> x / 2)" 2 2;
  test (fun x -> x / 2) "(fun x -> x / 2)" 2 1;
  testf (( *. ) 2.) "(( *. ) 2.)" 2. 5;
  testf (fun x -> x /. 2.) "(fun x -> x /. 2.)" 2. 3;
  testf (fun x -> x /. 2.) "(fun x -> x /. 2.)" 2. 2;
