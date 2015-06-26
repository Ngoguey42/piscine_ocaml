(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Calculator.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 14:20:24 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 14:27:08 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type CALC =
functor (M : ArithmeticMonoids.MONOID) ->
sig
  val add : M.element -> M.element -> M.element
  val sub : M.element -> M.element -> M.element
  val mul : M.element -> M.element -> M.element
  val div : M.element -> M.element -> M.element
  val power : M.element -> int -> M.element
  val fact : M.element -> M.element
end

module Calc : CALC =
functor (M : ArithmeticMonoids.MONOID) ->
struct
  
  let add a b: M.element = a
  let sub a b: M.element = a
  let mul a b: M.element = a
  let div a b: M.element = a
  let power a p: M.element = a
  let fact a: M.element = a
							
end
