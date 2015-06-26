(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 13:30:35 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 13:44:36 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let test a b =
  Printf.printf "add %+3d %+3d = %2d\t" a b (Watchtower.add a b);
  Printf.printf "sub %+3d %+3d = %2d\n%!" a b (Watchtower.sub a b)
  

let () =
  test 14 0;
  test 13 0;
  test 12 0;
  test 11 0;
  test 10 0;
  test 9 0;
  test 8 0;
  test 7 0;
  test 6 0;
  test 5 0;
  test 4 0;
  test 3 0;
  test 2 0;
  test 1 0;
  test 0 0;
  test ~-1 0;
  test ~-2 0;
  test ~-3 0;
  test ~-4 0;
  test ~-5 0;
  test ~-6 0;
  test ~-7 0;
  test ~-8 0;
  test ~-9 0;
  test ~-10 0;
  test ~-11 0;
  test ~-12 0;
  test ~-13 0;
  test ~-14 0;
  Printf.printf "\n%!";
  test 14 3;
  test 13 3;
  test 12 3;
  test 11 3;
  test 10 3;
  test 9 3;
  test 8 3;
  test 7 3;
  test 6 3;
  test 5 3;
  test 4 3;
  test 3 3;
  test 2 3;
  test 1 3;
  test 0 3;
  test ~-1 3;
  test ~-2 3;
  test ~-3 3;
  test ~-4 3;
  test ~-5 3;
  test ~-6 3;
  test ~-7 3;
  test ~-8 3;
  test ~-9 3;
  test ~-10 3;
  test ~-11 3;
  test ~-12 3;
  test ~-13 3;
  test ~-14 3;
  Printf.printf "\n%!";
  test 14 ~-3;
  test 13 ~-3;
  test 12 ~-3;
  test 11 ~-3;
  test 10 ~-3;
  test 9 ~-3;
  test 8 ~-3;
  test 7 ~-3;
  test 6 ~-3;
  test 5 ~-3;
  test 4 ~-3;
  test 3 ~-3;
  test 2 ~-3;
  test 1 ~-3;
  test 0 ~-3;
  test ~-1 ~-3;
  test ~-2 ~-3;
  test ~-3 ~-3;
  test ~-4 ~-3;
  test ~-5 ~-3;
  test ~-6 ~-3;
  test ~-7 ~-3;
  test ~-8 ~-3;
  test ~-9 ~-3;
  test ~-10 ~-3;
  test ~-11 ~-3;
  test ~-12 ~-3;
  test ~-13 ~-3;
  test ~-14 ~-3;
