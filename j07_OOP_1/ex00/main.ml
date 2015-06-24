(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 13:54:14 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/24 14:02:44 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
  Printf.printf "Tests:\n%!";
  let p1 = new People.people in
  Printf.printf "to_string: %s\n%!" (p1#to_string);
  Printf.printf "talk:\n%!";
  p1#talk;
  Printf.printf "die:\n%!";
  p1#die;
