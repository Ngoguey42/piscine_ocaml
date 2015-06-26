(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Set.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 17:37:52 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 18:12:20 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a t = 'a list

let rec is_in elt l =
  match l with
  | []						-> false
  | hd::tl when hd = elt	-> true
  | _::tl					-> is_in elt tl

									 
(* ’a -> ’a Set.t *)
let return v: 'a t =
  [v]

(* ’a Set.t -> (’a -> ’b Set.t) -> ’b Set.t *)
let bind (sa: 'a t) f: 'b t =
  let rec helper l =
	match l with
	| []						-> []
	| hd::tl					-> f hd @ helper tl
  in
  helper sa
		 
(* ’a Set.t -> ’a Set.t -> ’a Set.t *)
let union (sa: 'a t) (sb: 'a t): 'a t =
  let rec helper sb acc =
	match sb with
	| []						-> acc
	| hd::tl when is_in hd sa	-> helper tl acc
	| hd::tl					-> helper tl ([hd] @ acc)
  in
  sa @ helper sb []

(* ’a Set.t -> ’a Set.t -> ’a Set.t *)
let inter (sa: 'a t) (sb: 'a t): 'a t =
  let rec helper sa acc =
	match sa with
	| []						-> acc
	| hd::tl when is_in hd sb	-> helper tl ([hd] @ acc)
	| _::tl						-> helper tl acc
  in
  helper sa []

(* ’a Set.t -> ’a Set.t -> ’a Set.t  *)
let diff (sa: 'a t) (sb: 'a t): 'a t =
  let rec helper l l' acc =
	match l with
	| []						-> acc
	| hd::tl when is_in hd l'	-> helper tl l' acc
	| hd::tl					-> helper tl l' ([hd] @ acc)
  in
  helper sb sa [] @ helper sb sa []

(* ’a Set.t -> (’a -> bool) -> ’a Set.t *)
let filter (s: 'a t) f: 'a t =
  let rec helper l acc =
	match l with
	| []						-> acc
	| hd::tl when f hd			-> helper tl ([hd] @ acc)
	| _::tl						-> helper tl acc
  in
  helper s []

(* ’a Set.t -> (’a -> unit) -> unit *)
let foreach (s: 'a t) (f: 'a -> unit) =
  let rec helper l =
	match l with
	| []						-> ()
	| hd::tl					-> f hd;
								   helper tl
  in
  helper s

(* ’a Set.t -> (’a -> bool) -> bool *)
let for_all (s: 'a t) f =
  let rec helper l =
	match l with
	| []						-> true
	| hd::tl when f hd			-> helper tl
	| _							-> false
  in
  helper s
		 
(* ’a Set.t -> (’a -> bool) -> bool *)
let exists (s: 'a t) f =
  let rec helper l =
	match l with
	| []						-> false
	| hd::_ when f hd			-> true
	| hd::tl					-> helper tl
  in
  helper s



		 
