(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   doctor.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 15:05:38 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/24 16:17:02 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class doctor =
object
  initializer print_endline "doctor CTOR"
  val name = "unknown"
  val age = 1250
  val sidekick = new People.people
  val hp = 100
  method to_string = name
					 ^ ": " ^ (string_of_int age) ^ "y/o"
					 ^ ", " ^ (string_of_int hp) ^ "hp"
					 ^ ", sidekick(" ^ (sidekick#to_string) ^ ")"
  method talk = print_endline ("Hi! I'm the Doctor!")
  method travel_in_time start arrival =
	print_endline (""
				   ^ "   ____[  ]____\n"
				   ^ "  [ POLICE BOX ]  \n"
				   ^ "| [ ## ]  [ ## ] |\n"
				   ^ "| [ [] ]  [ oo ] |\n"
				   ^ "| [    ]  [    ] |\n"
				   ^ "| [    ]  [    ] |\n"
				   ^ "| [    ]  [    ] |\n"
				   ^ "| [    ]  [    ] |\n"
				   ^ "  ______________  \n");
	{< age = (arrival - start) + age >}
  method use_sonic_screwdriver =
	print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
  method private regenerate =
	{< hp = 100 >}	
end
