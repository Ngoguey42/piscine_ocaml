(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nucleotides.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/17 18:36:13 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/17 18:45:57 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide c: nucleotide =
  match c with
  | 'A'						-> "phosphate", "deoxyribose", A
  | 'T'						-> "phosphate", "deoxyribose", T
  | 'C'						-> "phosphate", "deoxyribose", C
  | 'G'						-> "phosphate", "deoxyribose", G
  | _						-> "phosphate", "deoxyribose", None
