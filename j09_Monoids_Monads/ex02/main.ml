(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 14:15:31 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 15:17:53 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Calc_int = Calculator.Calc(ArithmeticMonoids.INT)
module Calc_float = Calculator.Calc(ArithmeticMonoids.FLOAT)
let () =
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0))
