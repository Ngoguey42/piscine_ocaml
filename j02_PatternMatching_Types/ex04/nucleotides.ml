(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nucleotides.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 18:36:13 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/17 18:42:15 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide = function
  | 'A'						-> "phosphate", "deoxyribose", A
  | 'T'						-> "phosphate", "deoxyribose", T
  | 'C'						-> "phosphate", "deoxyribose", C
  | 'G'						-> "phosphate", "deoxyribose", G
  | _						-> "phosphate", "deoxyribose", None
