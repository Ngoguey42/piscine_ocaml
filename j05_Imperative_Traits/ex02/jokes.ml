(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/22 11:16:00 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/22 12:06:32 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let myjokes =
  ([|"What is the number one cause of divorce?: Marriage!"
	;"What do sprinters eat before a race?: Nothing, they fast."
	;"What do you call a masturbating cow?: Beef stroking off!"
	;"What do you call a masseuse who hates women?: A (massage)ynist!"
	;"What do you call the security guards outside of Samsung.: The guardians of the galaxy!"
	;"Where do skunks go to pray?: The pew!"|])

let () =
  let n = Array.length myjokes in
  Random.self_init ();
  print_endline (myjokes.(Random.int n))
