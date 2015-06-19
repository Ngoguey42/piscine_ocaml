(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Card.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 15:15:14 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/19 16:50:30 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Color = struct
	type t = Spade | Heart | Diamond | Club

	let all = [Spade; Heart; Diamond; Club]
				
	let toString (c: t) =
	  let string_of_t = function
		| Spade				-> "S"
		| Heart				-> "H"
		| Diamond			-> "D"
		| Club				-> "C"
	  in
	  string_of_t c

	let toStringVerbose (c: t) =
	  let string_of_t = function
		| Spade				-> "Spade"
		| Heart				-> "Heart"
		| Diamond			-> "Diamond"
		| Club				-> "Club"
	  in
	  string_of_t c
  end

module Value = struct
	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

	let toInt (c: t) =
	  let int_of_t = function
		| T2-> 1
		| T3-> 2
		| T4-> 3
		| T5-> 4
		| T6-> 5
		| T7-> 6
		| T8-> 7
		| T9-> 8
		| T10-> 9
		| Jack-> 10
		| Queen-> 11
		| King-> 12
		| As-> 13
	  in
	  int_of_t c

	let toString (c: t) =
	  let string_of_t = function
		| T2-> "2"
		| T3-> "3"
		| T4-> "4"
		| T5-> "5"
		| T6-> "6"
		| T7-> "7"
		| T8-> "8"
		| T9-> "9"
		| T10-> "10"
		| Jack-> "J"
		| Queen-> "Q"
		| King-> "K"
		| As-> "A"
	  in
	  string_of_t c

	let toStringVerbose (c: t) =
	  let string_of_t = function
		| T2-> "2"
		| T3-> "3"
		| T4-> "4"
		| T5-> "5"
		| T6-> "6"
		| T7-> "7"
		| T8-> "8"
		| T9-> "9"
		| T10-> "10"
		| Jack-> "Jack"
		| Queen-> "Queen"
		| King-> "King"
		| As-> "As"
	  in
	  string_of_t c

	let next (c: t) =
	  let next_of_t = function
		| T2-> T3
		| T3-> T4
		| T4-> T5
		| T5-> T6
		| T6-> T7
		| T7-> T8
		| T8-> T9
		| T9-> T10
		| T10-> Jack
		| Jack-> Queen
		| Queen-> King
		| King-> As
		| As-> invalid_arg "As"
	  in
	  next_of_t c

	let previous (c: t) =
	  let prev_of_t = function
		| T2-> invalid_arg "T2"
		| T3-> T2
		| T4-> T3
		| T5-> T4
		| T6-> T5
		| T7-> T6
		| T8-> T7
		| T9-> T8
		| T10-> T9
		| Jack-> T10
		| Queen-> Jack
		| King-> Queen
		| As-> King
	  in
	  prev_of_t c

  end

type t = Value.t * Color.t
					 
let newCard (v: Value.t) (c: Color.t): t =
  (v, c)

let allSpades : t list =
  [(Value.T2, Color.Spade); (Value.T3, Color.Spade); (Value.T4, Color.Spade);
   (Value.T5, Color.Spade); (Value.T6, Color.Spade); (Value.T7, Color.Spade);
   (Value.T8, Color.Spade); (Value.T9, Color.Spade); (Value.T10, Color.Spade);
   (Value.Jack, Color.Spade); (Value.Queen, Color.Spade);
   (Value.King, Color.Spade); (Value.As, Color.Spade)]
let allHearts : t list =
  [(Value.T2, Color.Heart); (Value.T3, Color.Heart); (Value.T4, Color.Heart);
   (Value.T5, Color.Heart); (Value.T6, Color.Heart); (Value.T7, Color.Heart);
   (Value.T8, Color.Heart); (Value.T9, Color.Heart); (Value.T10, Color.Heart);
   (Value.Jack, Color.Heart); (Value.Queen, Color.Heart);
   (Value.King, Color.Heart); (Value.As, Color.Heart)]	
let allDiamonds : t list =
  [(Value.T2, Color.Diamond); (Value.T3, Color.Diamond); (Value.T4, Color.Diamond);
   (Value.T5, Color.Diamond); (Value.T6, Color.Diamond); (Value.T7, Color.Diamond);
   (Value.T8, Color.Diamond); (Value.T9, Color.Diamond); (Value.T10, Color.Diamond);
   (Value.Jack, Color.Diamond); (Value.Queen, Color.Diamond);
   (Value.King, Color.Diamond); (Value.As, Color.Diamond)]
let allClubs : t list =
	[(Value.T2, Color.Club); (Value.T3, Color.Club); (Value.T4, Color.Club);
   (Value.T5, Color.Club); (Value.T6, Color.Club); (Value.T7, Color.Club);
   (Value.T8, Color.Club); (Value.T9, Color.Club); (Value.T10, Color.Club);
   (Value.Jack, Color.Club); (Value.Queen, Color.Club);
   (Value.King, Color.Club); (Value.As, Color.Club)]
let all : t list =
  List.append (List.append (List.append allSpades allHearts) allDiamonds) allClubs

let getValue ((v, _): t) =
  v
let getColor ((_, c): t) =
	c

let toString ((v, c): t) =
  Printf.sprintf "%s%s" (Value.toString v) (Color.toString c)
let toStringVerbose ((v, c): t) =
  Printf.sprintf "Card(%s, %s)" (Value.toStringVerbose v) (Color.toStringVerbose c)

let compare ((v, _): t) ((v', _): t) =
  (Value.toInt v) - (Value.toInt v')
let max ((v, _) as c: t) ((v', _) as c': t): t =
let diff = (Value.toInt v) - (Value.toInt v') in
  if diff >= 0 then
	c
  else
	c'
let min ((v, _) as c: t) ((v', _) as c': t): t =
  let diff = (Value.toInt v) - (Value.toInt v') in
  if diff <= 0 then
	c
  else
	c'
let best (l: t list) =
  match l with
  | [] 								-> invalid_arg "empty"
  | hd::tl							-> List.fold_left max hd tl

let isOf ((_, c): t) c' =
  (Color.toString c) = (Color.toString c')
let isSpade ((_, c): t) =
  (Color.toString c) = "S"
let isHeart ((_, c): t) =
  (Color.toString c) = "H"
let isDiamond ((_, c): t) =
  (Color.toString c) = "D"
let isClub ((_, c): t) =
  (Color.toString c) = "C"
