(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gardening.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 11:53:34 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/18 13:41:25 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree
											   
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

let draw_square x y size =
  draw_rectangle x y size size

let padding = 3
let paddingdouble = padding * 2
let rect_sizex = 50
let rect_sizexhalf = rect_sizex / 2
let text_lbound = rect_sizexhalf - padding
let text_height = 12
let text_height_half = text_height / 2
let rect_sizey = 20
let rect_sizeyhalf = rect_sizey / 2

let node1x = 100
let node1y = 150
				   
let draw_tree_node (t) =
  let draw_textbox str x y =
	draw_rectangle x y rect_sizex rect_sizey;
	Graphics.moveto (x - text_lbound) (y - text_height_half);
	Graphics.draw_string str
  in
  let draw_line x y x' y' =
	Graphics.moveto x y;
	Graphics.lineto x' y'
  in
  let draw_basic_node str =
	draw_textbox str node1x node1y;
	let box1rbound = node1x + rect_sizexhalf in
	let box2y = node1y - rect_sizeyhalf - paddingdouble in
	let box3y = node1y + rect_sizeyhalf + paddingdouble in
	let box23x = box1rbound + rect_sizex in
	draw_textbox "Nil" box23x box2y;
	draw_textbox "Nil" box23x box3y;
	let box23lx = node1x + rect_sizex in
	draw_line box1rbound node1y box23lx box2y;
	draw_line box1rbound node1y box23lx box3y
  in
  match t with
  | Nil					-> draw_textbox "Nil" node1x node1y
  | Node(v, Nil, Nil)	-> draw_basic_node v
  | _					-> ()
	
let () =
  Graphics.open_graph " 300x300";
  draw_square 5 5 10;
  draw_square 15 15 10;
  draw_square 20 20 30;
  draw_square 25 25 10;
  draw_square 35 35 10;
  (* draw_tree_node Nil; *)
  draw_tree_node (Node("Salut", Nil, Nil));
  ignore(read_line ());
  
