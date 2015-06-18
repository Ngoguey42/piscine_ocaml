(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   btree.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 18:19:55 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/18 19:25:49 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let is_bst t =
  let rec helper t pval first =
	match t with
	| Nil										-> true
	| Node(v, a, b) when first = true			->
	   (helper a v false) && (helper b v false)
	| Node(v, a, b) when a >= pval || b >= pval	-> false
	| Node(v, a, b) 							->
	   (helper a v false) && (helper b v false)
  in
  helper t 0 true

let is_perfect t =
  let rec helper = function
	| Nil | Node(_, Nil, Nil)					-> true
	| _											-> false
  in
  helper t

let is_balanced t =
  true

let search_bst t v =
  let rec helper = function
	| Nil										-> false
	| Node(v', _, _) when v = v' 				-> true
	| Node(v', a, _) when v < v' 				-> helper a
	| Node(_, _, b)								-> helper b
  in
  helper t

let add_bst t v =
  let rec helper = function
	| Node(v', _, _) when v = v'				-> failwith "already exists"; Nil
	| Node(v', a, b) when v < v'                -> Node(v', helper a, b)
	| Node(v', a, b)							-> Node(v', a, helper b)
	| _											-> Node(v, Nil, Nil)
  in
  helper t

let delete_bst t v =
  let rec min = function
	| Node(v', Nil, _)				  			-> v'
	| Node(_, a, _)								-> min a
	| _											-> failwith "unreachable"; v
  and merge a b =
	match a, b with
	| Nil, x | x, Nil							-> x
	| _											->let minb = min b in
												  Node(minb, a, helper b minb)
  and helper t v =
	match t with
	| Node(v', Nil, Nil) when v = v'			-> Nil
	| Node(v', a, b) when v = v'				-> merge a b
	| Node(v', a, b) when v < v'                -> Node(v', helper a, b)
	| Node(v', a, b)							-> Node(v', a, helper b)
	| _											-> failwith "doesn't exists"; Nil	  
  in
  helper t v										   
