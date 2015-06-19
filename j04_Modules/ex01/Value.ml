(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Value.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 14:35:16 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/19 16:52:05 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

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
