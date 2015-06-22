(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sum.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/22 12:07:53 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/22 12:15:58 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let sum a b =
  a +. b

let values = [|nan; infinity; neg_infinity; +.0.; -.0.; +.42.42; -.42.42|]
			   
let () =
  let n = Array.length values in
  for i = 0 to n - 1 do
	for j = 0 to n - 1 do
	  let a = values.(i) in
	  let b = values.(j) in
	  Printf.printf "sum %+10f %+-10f = %!" a b;
	  Printf.printf "%+f\n%!" (sum a b);
	done
  done
