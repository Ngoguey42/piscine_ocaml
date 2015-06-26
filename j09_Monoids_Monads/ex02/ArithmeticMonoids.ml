(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ArithmeticMonoids.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 14:20:48 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 14:29:00 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type MONOID =
  sig
	type element
	val zero1 : element
	val zero2 : element
	val mul : element -> element -> element
	val add : element -> element -> element
	val div : element -> element -> element
	val sub : element -> element -> element
  end

module INT : MONOID =
  struct
	type element = int
	let zero1: element = 0
	let zero2: element = 1
	let mul (a: element) (b: element): element = a * b
	let add (a: element) (b: element): element = a + b
	let div (a: element) (b: element): element = a / b
	let sub (a: element) (b: element): element = a - b
					 
  end
