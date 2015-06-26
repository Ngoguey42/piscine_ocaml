(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Calculator.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 14:20:24 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 15:32:58 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* module type CALC = *)
(* functor (M : ArithmeticMonoids.MONOID) -> *)
(* sig *)
(*   val add : M.element -> M.element -> M.element *)
(*   val sub : M.element -> M.element -> M.element *)
(*   val mul : M.element -> M.element -> M.element *)
(*   val div : M.element -> M.element -> M.element *)
(*   val power : M.element -> int -> M.element *)
(*   val fact : M.element -> M.element *)
(* end *)

module Calc =
functor (M : ArithmeticMonoids.MONOID) ->
struct
  
  let add a b: M.element = M.add a b
  let sub a b: M.element = M.sub a b
  let mul a b: M.element = M.mul a b
  let div a b: M.element = M.div a b

  let power a p: M.element =
	(* P MUST ALWAYS BE POSITIVE (subject) *)
	let rec helper p acc =
	  match p with
	  | 0				-> acc
	  | _				-> helper (p - 1) (M.mul acc a)
	in
	helper p M.zero2

  let fact a: M.element =
	(* A MUST BE POSITIVE *)
	let rec helper v acc =
	  if v > M.zero1 then
		helper (M.sub v M.zero2) (mul acc v)
	  else
		acc
	in
	helper a M.zero2
							
end
