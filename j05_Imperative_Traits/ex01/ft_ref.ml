(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ref.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/22 10:52:53 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/22 11:13:43 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a ft_ref = {mutable data: 'a}

let return v: 'a ft_ref =
  {data = v}

let get ({data}: 'a ft_ref) =
  data

let set (r: 'a ft_ref) v=
  r.data <- v

let bind ({data}: 'a ft_ref) f: 'b ft_ref =
  f data
	  
let () =
  Printf.printf "\n%!";
  Printf.printf "Saving an int into an ft_ref through 'return': \nlet save = return 18 in\n%!";
  let save = return 18 in
  Printf.printf "\027[35msave.data = %d\027[0m\n%!" save.data;
  Printf.printf "\027[35mget save = %d\027[0m\n%!" (get save);
  
  Printf.printf "Updating save's data through 'set': \nset save 81\n%!";
  set save 81;
  Printf.printf "\027[35msave.data = %d\027[0m\n%!" save.data;
  Printf.printf "\027[35mget save = %d\027[0m\n%!" (get save);
  
  Printf.printf "Saving this int as a string through 'bind':\nlet savestr = bind save (fun i-> return (string_of_int i)) in\n%!";
  let savestr = bind save (fun i-> return (string_of_int i)) in
  Printf.printf "\027[35msave.data = %d\027[0m\n%!" save.data;
  Printf.printf "\027[35mget save = %d\027[0m\n%!" (get save);
  Printf.printf "\027[36msavestr.data = %s\027[0m\n%!" savestr.data;
  Printf.printf "\027[36mget savestr = %s\027[0m\n%!" (get savestr);
  Printf.printf "Modifing savestr value:\nset savestr \"kikou\"\n%!";
  set savestr "kikou";
  Printf.printf "\027[36msavestr.data = %s\027[0m\n%!" savestr.data;
  Printf.printf "\027[36mget savestr = %s\027[0m\n%!" (get savestr);
  Printf.printf "\n%!";
  
  
