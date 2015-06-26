(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 14:15:31 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 15:35:10 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Calc_int = Calculator.Calc(ArithmeticMonoids.INT)
module Calc_float = Calculator.Calc(ArithmeticMonoids.FLOAT)

let test a b =
  let a' = int_of_float a in
  let b' = int_of_float b in
  Printf.printf "\027[35ma:%-+6.2f b:%+-6.2f\t\ta':%-+6d b':%-+6d\027[0m <=VALUES\n%!"
				a b a' b';
  Printf.printf "add: %-+12.2f\t\tadd: %-+12d\n%!" (Calc_float.add a b) (Calc_int.add a' b');
  Printf.printf "sub: %-+12.2f\t\tsub: %-+12d\n%!" (Calc_float.sub a b) (Calc_int.sub a' b');
  Printf.printf "mul: %-+12.2f\t\tmul: %-+12d\n%!" (Calc_float.mul a b) (Calc_int.mul a' b');
  Printf.printf "div: %-+12.2f\t\tdiv: %-+12d\n%!" (Calc_float.div a b) (Calc_int.div a' b');
  Printf.printf "a:%-+6.2f b:%+-6.2f\t\ta':%-+6d b':%-+6d <=POWER 0\n%!"
				(Calc_float.power a 0) (Calc_float.power b 0)
				(Calc_int.power a' 0) (Calc_int.power b' 0);
  Printf.printf "a:%-+6.2f b:%+-6.2f\t\ta':%-+6d b':%-+6d <=POWER 1\n%!"
				(Calc_float.power a 1) (Calc_float.power b 1)
				(Calc_int.power a' 1) (Calc_int.power b' 1);
  Printf.printf "a:%-+6.2f b:%+-6.2f\t\ta':%-+6d b':%-+6d <=POWER 2\n%!"
				(Calc_float.power a 2) (Calc_float.power b 2)
				(Calc_int.power a' 2) (Calc_int.power b' 2);
  Printf.printf "a:%-+6.2f b:%+-6.2f\t\ta':%-+6d b':%-+6d <=FACT\n%!"
		(Calc_float.fact a) (Calc_float.fact b) (Calc_int.fact a') (Calc_int.fact b') ;
  Printf.printf "\n%!"

let () =
  test 0. 0.;  
  test 1. 1.;  
  test ~-.1. ~-.1.;
  test 1. 0.;
  test 0. 1.;  
  test ~-.1. 0.;
  test 0. ~-.1.;
  test ~-.1. 1.;
  test 1. ~-.1.;
  test 4.4242 ~-.100.;  
