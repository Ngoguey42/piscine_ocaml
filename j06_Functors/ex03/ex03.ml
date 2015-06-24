(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex03.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 13:50:07 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/24 19:54:45 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type FIXED = sig
	type t
	val of_float : float -> t
	val of_int : int -> t
	val to_float : t -> float
	val to_int : t -> int
	val to_string : t -> string
	val zero : t
	val one : t
	val succ : t -> t
	val pred : t -> t
	val min : t -> t -> t
	val max : t -> t -> t
	val gth : t -> t -> bool
	val lth : t -> t -> bool
	val gte : t -> t -> bool
	val lte : t -> t -> bool
	val eqp : t -> t -> bool
	val eqs : t -> t -> bool
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val foreach : t -> t -> (t -> unit) -> unit
  end

module type FRACTIONNAL_BITS =
  sig
	val bits : int
  end

module type MAKE =
  functor (Truc: FRACTIONNAL_BITS) -> FIXED
										
module Make: MAKE =
  functor (Truc: FRACTIONNAL_BITS) ->
  struct
	type t = int
	let base = 1 lsl Truc.bits
	let basef = float_of_int base
							 
	let of_float v1 =
	  let v1' = v1 *. basef in
	  if v1' < 0. then
		int_of_float (v1' -. 0.5)
	  else
		int_of_float (v1' +. 0.5)
	(* float -> t *)
					 
	let of_int v1 = v1 * base
	(* int -> t *)

	let to_float v1 = float_of_int (v1) /. basef
	(* t -> float *)

	let to_int v1 = v1 / base
	(* t -> int *)

	let to_string v1 = string_of_float (to_float v1)
	(* t -> string *)

	let zero = 0
	(* t *)

	let one = base
	(* t *)

	let succ v1 = v1 + 1
	(* t -> t *)

	let pred v1 = v1 - 1
	(* t -> t *)

	let min v1 v2 = if v1 < v2 then v1 else v2
	(* t -> t -> t *)

	let max v1 v2 = if v1 > v2 then v1 else v2
	(* t -> t -> t *)

	let gth v1 v2 = v1 > v2
	(* t -> t -> bool *)

	let lth v1 v2 = v1 < v2
	(* t -> t -> bool *)

	let gte v1 v2 = v1 >= v2
	(* t -> t -> bool *)

	let lte v1 v2 = v1 <= v2
	(* t -> t -> bool *)

	let eqp v1 v2 = v1 = v2
	(* t -> t -> bool *)

	let eqs v1 v2 = v1 == v2
	(* t -> t -> bool *)

	let add v1 v2 = v1 + v2
	(* t -> t -> t *)

	let sub v1 v2 = v1 - v2
	(* t -> t -> t *)

	let mul v1 v2 =
	  int_of_float ((to_float v1) *. (float_of_int v2))
	(* t -> t -> t *)

	let div v1 v2 =
	  of_float ((to_float v1) /. (to_float v2))
	(* t -> t -> t *)

	let rec foreach v1 v2 f =
	  if v1 < v2 then
		begin
		  f v1;
		  foreach (v1 + 1) v2 f
		end
	  else if v1 > v2 then
		begin
		  f v1;
		  foreach (v1 - 1) v2 f
		end
	  else
		f v1;
	  (* t -> t -> (t -> unit) -> unit *)
  end
	
module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let test2t a f sf =
  Printf.printf "Test with [%s %f] = %f\n%!"
				sf (Fixed8.to_float a) (Fixed8.to_float (f a))

let test3t a b f sf =
  Printf.printf "Test with [%s %f %f] = %f\n%!"
				sf (Fixed8.to_float a) (Fixed8.to_float b) (Fixed8.to_float (f a b))
				
let test2t1b a b f sf =
  Printf.printf "Test with [%s %f %f] = %B\n%!"
				sf (Fixed8.to_float a) (Fixed8.to_float b) ((f a b))
				
let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
  Fixed4.foreach (Fixed4.one) (Fixed4.zero) (fun f -> print_endline (Fixed4.to_string f));
  let a = Fixed8.of_float ~-.2.12 in 
  let b = Fixed8.of_int 42 in
  test2t a Fixed8.succ "succ";
  test2t a Fixed8.pred "pred";
  test3t a b Fixed8.min "min";
  test3t a b Fixed8.max "max";
  test2t1b a b Fixed8.gth "gth";
  test2t1b a b Fixed8.lth "lth";
  test2t1b b b Fixed8.gte "gte";
  test2t1b b b Fixed8.lte "lte";
  test2t1b a a Fixed8.eqp "eqp";
  test2t1b a a Fixed8.eqs "eqs";
  test3t a b Fixed8.add "add";
  test3t a b Fixed8.sub "sub";
  test3t a b Fixed8.mul "mul";
  test3t a b Fixed8.div "div"
