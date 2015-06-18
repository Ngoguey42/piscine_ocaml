(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_graphics.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 13:55:02 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/18 15:42:51 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

(* TREE OPERATION FUNCTIONS *)
let size (t) =
  let rec helper t acc =
	match t with
	| Nil					-> acc
	| Node (_, a, b)		-> helper a (acc + 1 + helper b 0)	
  in
  helper t 0
		 
let height (t) =
  let rec helper t acc =
	match t with
	| Nil					-> acc
	| Node (_, a, b)		-> max (helper a (acc + 1)) (helper b (acc + 1))
  in
  helper t 0

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
let winy = 1000			 

let padding = 3
let paddingdouble = padding * 2
let rect_sizex = 50
let rect_sizexhalf = rect_sizex / 2
let text_lbound = rect_sizexhalf - padding
let text_height = 12
let text_height_half = text_height / 2
let rect_sizey = 20
let rect_sizeyhalf = rect_sizey / 2

let rect_xpadding = rect_sizexhalf
let rect_ypadding = paddingdouble

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
  draw_textbox str x y;
  if link then
	begin
	  let lx = x + rect_sizexhalf in
	  let rx = lx + rect_xpadding in
	  let ty = y + ypad in
	  let by = y - ypad in
	  draw_line lx y rx ty;
	  draw_line lx y rx by
	end
	  
let draw_tree t =
  let rec helper t x y =
	match t with
	| Nil					-> draw_node "Nil" x y false 0
	| Node (v, a, b)		->
	   let s = height t in
	   let spower = int_of_float (2. ** float (s - 1)) in
	   let ypad = spower * (rect_sizeyhalf + rect_ypadding) in
	   draw_node v x y true ypad;
	   let x' = x + rect_sizex + rect_xpadding in
	   helper a x' (y + ypad);
	   helper b x' (y - ypad)
  in
  helper t 100 (winy / 2)

let testsize t =
  Printf.printf "Test: size=%02d height=%02d \n%!" (size t) (height t)
				
let () =
  Graphics.open_graph (Printf.sprintf " %dx%d" winx winy);
  let tr0 = Nil in
  let tr1 = Node("tr1", Nil, Nil) in
  let tr2 = Node("tr2", tr1, Nil) in
  let tr3 = Node("tr3", Nil, tr1) in
  let tr4 = Node("tr4", tr1, tr1) in
  let tr5 = Node("tr5", tr4, tr4) in
  let tr6 = Node("tr6", tr5, tr4) in
  testsize tr0;
  testsize tr1;
  testsize tr2;
  testsize tr3;
  testsize tr4;
  testsize tr5;
  (* draw_tree tr0; *)
  (* draw_tree tr1; *)
  (* draw_tree tr2; *)
  (* draw_tree tr3; *)
  (* draw_tree tr4; *)
  (* draw_tree tr5; *)
  draw_tree tr6;
  ignore(read_line ());
  
