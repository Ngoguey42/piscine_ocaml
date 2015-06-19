(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   btree.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 18:19:55 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/19 12:46:40 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let height t =
  let rec helper t acc =
	match t with
	| Nil-> acc
	| Node (_, a, b)-> max (helper a (acc + 1)) (helper b (acc + 1))
  in
  helper t 0

let is_bst t =
  let rec helper = function
	| Nil										-> true
	| Node(v, _, Node(v', _, _)) when v' < v	-> false
	| Node(v, Node(v', _, _), _) when v' > v	-> false
	| Node(_, a, b)								-> (helper a) && (helper b)
  in
  helper t

(* IS_PERFECT = IS_BALANCE WITH (DELTA = 0) *)
let is_perfect t =
  let rec helper = function
	| Nil										-> true
	| Node(_, a, b) when height a = height b	-> (helper a) && (helper b)
	| _											-> false
  in
  helper t

let is_balanced t =
  let rec helper = function
	| Nil										-> true
	| Node(_, a, b)
		 when abs ((height a) - (height b)) > 1	-> false
	| Node(_, a, b)								-> (helper a) && (helper b)
  in
  helper t
																 
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
	| Node(v', _, _) when v = v'				-> failwith "already exists"
	| Node(v', a, b) when v < v'                -> Node(v', helper a, b)
	| Node(v', a, b)							-> Node(v', a, helper b)
	| _											-> Node(v, Nil, Nil)
  in
  helper t

let delete_bst t v =
  let rec min = function
	| Node(v', Nil, _)				  			-> v'
	| Node(_, a, _)								-> min a
	| _											-> failwith "unreachable"
  and merge a b =
	match a, b with
	| Nil, x | x, Nil							-> x
	| _											-> let minb = min b in
												   Node(minb, a, helper b minb)
  and helper t v =
	match t with
	| Node(v', Nil, Nil) when v = v'			-> Nil
	| Node(v', a, b) when v = v'				-> merge a b
	| Node(v', a, b) when v < v'                -> Node(v', helper a v, b)
	| Node(v', a, b)							-> Node(v', a, helper b v)
	| _											-> failwith "doesn't exists"
  in
  helper t v										   



(* TEST *)
(* TEST *)
(* TEST *)
(* TEST *)
(* TEST *)
(* TEST *)
(* TEST *)
(* TEST *)
(* TEST *)
(* TEST *)

(* BASIC GRAPHIC OPERATIONS *)
let draw_rectangle x y xsize ysize =
  let sizehalfx = xsize / 2 in
  let sizerestx = xsize - sizehalfx in
  let sizehalfy = ysize / 2 in
  let sizeresty = ysize - sizehalfy in
  let xl = x - sizehalfx in
  let xr = x + sizerestx in
  let yt = y - sizehalfy in
  let yb = y + sizeresty in
  if xl >= 0 && xr >= 0 && yt >= 0 && yb >= 0 then
	begin
	  Graphics.moveto xl yt;
	  Graphics.lineto xr yt;
	  Graphics.lineto xr yb;
	  Graphics.lineto xl yb;
	  Graphics.lineto xl yt
	end

(* GRAPHIC CONSTANTS *)
let winx = 1000
let winy = 1600

let padding = 3
let paddingdouble = padding * 2
let rect_sizex = 50
let rect_sizexhalf = rect_sizex / 2
let text_lbound = rect_sizexhalf - padding
let text_height = 12
let text_height_half = text_height / 2
let rect_sizey = 12
let rect_sizeyhalf = rect_sizey / 2

let rect_xpadding = rect_sizexhalf
let rect_ypadding = 1

let node1x = 100
let node1y = 150

let draw_node str x y link ypad =
  let draw_textbox str x y =
	draw_rectangle x y rect_sizex rect_sizey;
	Graphics.moveto (x - text_lbound) (y - text_height_half);
	Graphics.draw_string str
  in
  let draw_line x y x' y' =
	Graphics.moveto x y;
	Graphics.lineto x' y'
  in
  if link then
	begin
	  draw_textbox (string_of_int str) x y;
	  let lx = x + rect_sizexhalf in
	  let rx = lx + rect_xpadding in
	  let ty = y + ypad in
	  let by = y - ypad in
	  draw_line lx y rx ty;
	  draw_line lx y rx by
	end
  else
	draw_textbox "nil" x y
	  
let draw_tree t =
  let rec helper t x y =
	match t with
	| Nil-> draw_node (-99999) x y false 0
	| Node (v, a, b)->
	   let s = height t in
	   let spower = int_of_float (2. ** float (s - 1)) in
	   let ypad = spower * (rect_sizeyhalf + rect_ypadding) in
	   draw_node v x y true ypad;
	   let x' = x + rect_sizex + rect_xpadding in
	   helper a x' (y + ypad);
	   helper b x' (y - ypad)
  in
  helper t 100 (winy / 2 - 300)

let test t =
  Printf.printf "is_bst: %B\n%!" (is_bst t);
  Printf.printf "is_perfect: %B\n%!" (is_perfect t);
  Printf.printf "is_balanced: %B\n%!" (is_balanced t);
  let lol = 8 in Printf.printf "search_bst [%d]: %B\n%!" lol (search_bst t lol);
  let lol = 10 in Printf.printf "search_bst [%d]: %B\n%!" lol (search_bst t lol);
  let lol = 12 in Printf.printf "search_bst [%d]: %B\n%!" lol (search_bst t lol);
  let lol = 14 in Printf.printf "search_bst [%d]: %B\n%!" lol (search_bst t lol);
  let lol = 15 in Printf.printf "search_bst [%d]: %B\n%!" lol (search_bst t lol);
  let lol = 16 in Printf.printf "search_bst [%d]: %B\n%!" lol (search_bst t lol);
  let lol = 17 in Printf.printf "search_bst [%d]: %B\n%!" lol (search_bst t lol);
  let lol = 18 in Printf.printf "search_bst [%d]: %B\n%!" lol (search_bst t lol);
  let lol = 19 in Printf.printf "search_bst [%d]: %B\n%!" lol (search_bst t lol);
  Graphics.open_graph (Printf.sprintf " %dx%d" winx winy);
  draw_tree t

let () =

  let tr0 = Nil in
  (* delete_bst tr0 10; *)
  let tr1 = add_bst tr0 20 in
  (* add_bst tr1 20; *)
  let tr2 = delete_bst tr1 20 in
  let tr3 = add_bst (add_bst tr2 20) 22 in
  let tr4 = add_bst tr3 10 in
  let tr5 = add_bst (add_bst (add_bst (add_bst tr4 14) 8) 18) 12 in
  let tr6 = add_bst tr5 19 in
  let tr7 = add_bst (add_bst (add_bst tr6 17) 15) 16 in
  let tr8 = delete_bst tr7 14 in
  
  (* EMPTY *)
  test tr0;

  (* ADDED 20 *)
  (* test tr1; *)

  (* REMOVED 20 *)
  (* test tr2; *)

  (* ADDED 20 AND 22 *)
  (* test tr3; *)

  (* ADDED 10 *)
  (* test tr4; *)

  (* ADDED 14 THEN 8 THEN 18 THEN 12*)
  (* test tr5; *)

  (* ADDED 19 *)
  (* test tr6; *)
  
  (* ADDED 17 THEN 15 THEN 16 *)
  (* test tr7; *)

  (* REMOVED 14 *)
  (* test tr8; *)
  
  ignore(read_line ());
