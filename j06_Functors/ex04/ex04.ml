(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex04.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 15:34:56 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/23 16:07:01 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* VAL SIG (subject) *)
module type VAL =
  sig
	type t
	val add : t -> t -> t
	val mul : t -> t -> t
  end

(* EVALEXPR SIG *)	
module type EVALEXPR =
  sig
	type t
	type expr = Value of t | Mult of expr * expr | Add of expr * expr
  end

(* MAKER SIG *)
module type MAKEEVALEXPR =
  functor (Truc: VAL) ->
  EVALEXPR with type t = Truc.t

(* MAKER IMPLEMENTATION *)
module MakeEvalExpr : MAKEEVALEXPR =
  functor (Truc: VAL) ->
  struct
	type t = Truc.t
	type expr = Value of t | Mult of expr * expr | Add of expr * expr
  end
	
(* VAL IMPLEMENTATIONS (subject) *)
module IntVal : VAL =
  struct
	type t = int
	let add = ( + )
	let mul = ( * )
  end
module FloatVal : VAL =
  struct
	type t = float
	let add = ( +. )
	let mul = ( *. )
  end
module StringVal : VAL =
  struct
	type t = string
	let add s1 s2 = if (String.length s1) > (String.length s2) then s1 else s2
	let mul = ( ^ )
  end
module IntEvalExpr : EVALEXPR = MakeEvalExpr (IntVal)
module FloatEvalExpr : EVALEXPR = MakeEvalExpr (FloatVal)
module StringEvalExpr : EVALEXPR = MakeEvalExpr (StringVal)
let ie = IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2)
(* let fe = FloatEvalExpr.Add (FloatEvalExpr.Value 41.5, FloatEvalExpr.Value 0.92) *)
(* let se = StringEvalExpr.Mul (StringEvalExpr.Value "very ", *)
(* 							 (StringEvalExpr.Add (StringEvalExpr.Value "very long", *)
(* 												  StringEvalExpr.Value "short"))) *)
(* let () = Printf.printf "Res = %d\n" (IntEvalExpr.eval ie) *)
(* let () = Printf.printf "Res = %f\n" (FloatEvalExpr.eval fe) *)
(* let () = Printf.printf "Res = %s\n" (StringEvalExpr.eval se) *)
