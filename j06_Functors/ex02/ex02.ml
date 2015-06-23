(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex02.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 13:19:47 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/23 13:37:31 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type PAIR = sig val pair : (int * int) end
module type VAL = sig val x : int end
					
(* FIX ME !!! *)
					
module Pair : PAIR = struct let pair = ( 21, 42 ) end

module type MAKEPROJECTION =
  functor (Truc : PAIR) -> VAL
	
module MakeFst : MAKEPROJECTION =
  functor (Truc : PAIR) ->
  struct
	let x = fst Truc.pair
  end
	
module MakeSnd : MAKEPROJECTION =
  functor (Truc : PAIR) ->
  struct
	let x = snd Truc.pair
  end
					   
module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)
let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
