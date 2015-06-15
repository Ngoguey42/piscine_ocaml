(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_alphabet.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/15 09:54:45 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/15 09:58:35 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec loop i =
  print_char (char_of_int i);
  if i < int_of_char 'z' then
	loop (i + 1)

let ft_print_alphabet () =
  loop (int_of_char 'a');
  print_char '\n'
		 
let () =
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ();
  ft_print_alphabet ()
