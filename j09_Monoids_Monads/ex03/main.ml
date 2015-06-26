(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/26 15:41:35 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/26 16:42:49 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let int_of_failure e =
  match e with
  | Try.Salut					-> Try.Success 42
  | Division_by_zero			-> Try.Success 84
  | _							-> Try.Success ~-42

let () =
  Printf.printf "let vt = Try.return 0 in\n%!";
  let vt = Try.return 0 in
  Try.print_t_int vt;
  
  Printf.printf "let vt2 = Try.bind vt (fun n -> Try.return(42 / n)) in\n%!";
  let vt2 = Try.bind vt (fun n -> Try.return(42 / n)) in
  Try.print_t_int vt2;

  Printf.printf "let vt3 = Try.recover vt2 int_of_failure in\n%!";
  let vt3 = Try.recover vt2 int_of_failure in
  Try.print_t_int vt3;
  
  Printf.printf "let vt4 = Try.filter vt3 (fun _ -> false) in\n%!";
  let vt4 = Try.filter vt3 (fun _ -> false) in
  Try.print_t_int vt4;

  Printf.printf "let vtt1 = Try.Success vt4 in\n%!";
  let vtt1 = Try.Success vt4 in  
  Printf.printf "let vt5 = Try.flatten vtt1 in\n%!";
  let vt5 = Try.flatten vtt1 in
  Try.print_t_int vt5;
  
  Printf.printf "let vtt2 = Try.Success vt3 in\n%!";
  let vtt2 = Try.Success vt3 in  
  Printf.printf "let vt6 = Try.flatten vtt2 in\n%!";
  let vt6 = Try.flatten vtt2 in
  Try.print_t_int vt6;
  
  Printf.printf "\n%!";
  
