(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   army.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/24 16:59:39 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/24 17:17:26 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class ['a] army =
object
  val l : 'a list = []
  method add n = {<l = n::l>}
  method delete =
  	match l with
  	| []		-> failwith "list is empty"
  	| _::tl		-> {<l = tl>}
  method get_l = l
end
