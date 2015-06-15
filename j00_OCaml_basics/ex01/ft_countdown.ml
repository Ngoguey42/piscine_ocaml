(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 09:19:27 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/15 09:35:24 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec down i =
  if i >= 0 then
	begin
	  print_int i;
	  print_char '\n';
	  down (i - 1)
	end
	  
let rec ft_countdown i =
  if i <= 0 then
	down 0
  else
	down i

let test i =
  Printf.printf "Test with [%d]:\n%!" i;
  ft_countdown i			   
			   
let () =
  test 12;
  test 1;
  test 0;
  test (-1);
  test (-42)
