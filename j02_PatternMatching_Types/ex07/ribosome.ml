(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   rna.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 10:01:49 by ngoguey           #+#    #+#             *)
(*   Updated: 2015/06/18 11:28:57 by ngoguey          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide = phosphate * deoxyribose * nucleobase
type helix = nucleotide list
type rna = nucleobase list
type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His |
				 Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val
type protein = aminoacid list
						 
let rec revl src dst =
  match src with
  | []-> dst
  | hd::tl-> revl tl (hd::dst)
				  
let generate_nucleotide c: nucleotide =
  match c with
  | 'A'						-> "phosphate", "deoxyribose", A
  | 'T'						-> "phosphate", "deoxyribose", T
  | 'C'						-> "phosphate", "deoxyribose", C
  | 'G'						-> "phosphate", "deoxyribose", G
  | _						-> "phosphate", "deoxyribose", None

let generate_helix n: helix =
  let get_nucleotide = function
	| 0						-> generate_nucleotide 'A'
	| 1						-> generate_nucleotide 'T'
	| 2						-> generate_nucleotide 'C'
	| _						-> generate_nucleotide 'G'
  in
  let rec generate_helix_helper n l =
	if n = 0 then
	  l
	else
	  generate_helix_helper (n - 1) (get_nucleotide (Random.int 4)::l)
  in
  Random.self_init ();
  if n < 0 then
	[]
  else
	generate_helix_helper n []

let helix_to_string (l: helix) =
  let base_to_string = function
	| A						-> "A"
	| T						-> "T"
	| C						-> "C"
	| G						-> "G"
	| _						-> "None"
  in
  let nucleotide_to_str (p, d, b) =
	p ^ " " ^ d ^ " " ^ base_to_string b ^ "; "
  in
  let rec helix_to_string_helper l s =
	match l with
	| []				-> s
	| hd::tl			-> helix_to_string_helper tl (s ^ nucleotide_to_str hd)
  in
  helix_to_string_helper l ""
						 
let complementary_helix (l: helix): helix =
  let complementary_base = function
	| A						-> T
	| T						-> A
	| C						-> G
	| G						-> C
	| _						-> None
  in
  let rec complementary_helix_helper l l' =
	match l with
	| []					-> l'
	| (p, d, b)::tl			-> complementary_helix_helper
								 tl ((p, d, complementary_base b) :: l')
  in
  revl (complementary_helix_helper l []) []

let generate_rna (l : helix): rna =
  let complementary_base = function
	| A						-> U
	| T						-> A
	| C						-> G
	| G						-> C
	| _						-> None
  in
  let rec generate_rna_helper l l' =
	match l with
	| []					-> l'
	| (p, d, b)::tl			-> generate_rna_helper
								 tl (complementary_base b :: l')
  in
  revl (generate_rna_helper l []) []
	   
let rna_to_string (l: rna) =
  let base_to_string = function
	| A						-> "A"
	| U						-> "U"
	| C						-> "C"
	| G						-> "G"
	| _						-> "None"
  in
  let rec rna_to_string_helper l s =
	match l with
	| []					-> s
	| hd::tl				-> rna_to_string_helper tl (s ^ base_to_string hd)
  in
  rna_to_string_helper l ""
					   
let generate_bases_triplets (l: rna) =
  let rec helper l l' =
	match l with
	| hd1::hd2::hd3::tl		-> helper tl ((hd1, hd2, hd3)::l')
	| _						-> l'
  in
  (* helper l [] *)
  revl (helper l []) []

let triplet_to_string l =
  let base_to_string = function
	| A						-> "A"
	| U						-> "U"
	| C						-> "C"
	| G						-> "G"
	| _						-> "None"
  in
  let to_string (a, b, c) =
	base_to_string a ^ base_to_string b ^ base_to_string c ^ "; "
  in
  let rec triplet_to_string_helper l s =
	match l with
	| []					-> s
	| hd::tl				-> triplet_to_string_helper tl (s ^ to_string hd)
  in
  triplet_to_string_helper l ""

let string_of_protein (l:protein) =
  let string_of_aa = function
	| Stop-> "End of translation"
	| Ala-> "Alanine"
	| Arg-> "Arginine"
	| Asn-> "Asparagine"
	| Asp-> "Aspartique"
	| Cys-> "Cysteine"
	| Gln-> "Glutamine"
	| Glu-> "Glutamique"
	| Gly-> "Glycine"
	| His-> "Histidine"
	| Ile-> "Isoleucine"
	| Leu-> "Leucine"
	| Lys-> "Lysine"
	| Met-> "Methionine"
	| Phe-> "Phenylalanine"
	| Pro-> "Proline"
	| Ser-> "Serine"
	| Thr-> "Threonine"
	| Trp-> "Tryptophane"
	| Tyr-> "Tyrosine"
	| Val-> "Valine"
  in
  let to_string p =
	string_of_aa p ^ "; "
  in
  let rec helper l s =
	match l with
	| []					-> s
	| hd::tl				-> helper tl (s ^ to_string hd)
  in
  helper l ""

let decode_arn (l : rna) : protein =
  let aminoacid_from_triplet = function
	| G, C, A | G, C, C | G, C, G | G, C, U-> Ala
	| A, G, A | A, G, G | C, G, A | C, G, C | C, G, G | C, G, U-> Arg
	| A, A, C | A, A, U-> Asn
	| G, A, C | G, A, U-> Asp
	| U, G, C | U, G, U-> Cys
	| C, A, A | C, A, G-> Gln
	| G, A, A | G, A, G-> Glu
	| G, G, A | G, G, C | G, G, G | G, G, U-> Gly
	| C, A, C | C, A, U-> His
	| A, U, A | A, U, C | A, U, U-> Ile
	| C, U, A | C, U, C | C, U, G | C, U, U | U, U, A | U, U, G-> Leu
	| A, A, A | A, A, G-> Lys
	| A, U, G-> Met
	| U, U, C | U, U, U-> Phe
	| C, C, C | C, C, A | C, C, G | C, C, U-> Pro
	| U, C, A | U, C, C | U, C, G | U, C, U | A, G, U | A, G, C-> Ser
	| A, C, A | A, C, C | A, C, G | A, C, U-> Thr
	| U, G, G-> Trp
	| U, A, C | U, A, U-> Tyr
	| G, U, A | G, U, C | G, U, G | G, U, U-> Val
	| U, A, A | U, A, G | U, G, A | _ -> Stop
  in
  let rec helper l l' =
	match l with
	| []					-> l'
	| hd::tl				->
	   begin
		 let aa = aminoacid_from_triplet hd in
		 if aa = Stop then
		   aa::l'
		 else
		   helper tl (aa::l')
	   end
  in
  revl (helper (generate_bases_triplets l) []) []
	   
let test n =
  let l = generate_helix n in
  let s = helix_to_string l in
  Printf.printf "Test with [n = %d]:\n     generate+tostring: \027[35m%s\027[0m\n%!" n s;
  let l' = generate_rna l in
  let s' = rna_to_string l' in
  Printf.printf " generate_rna+tostring: \027[35m%s\027[0m\n%!" s';
  let l'' = generate_bases_triplets l' in
  let s'' = triplet_to_string l'' in
  Printf.printf "generate_trip+tostring: \027[35m%s\027[0m\n%!" s'';
  let l''' = decode_arn l' in
  let s''' = string_of_protein l''' in
  Printf.printf "   decode_arn+tostring: \027[35m%s\027[0m\n%!" s''';
  Printf.printf "\n%!"

let test2 l =
  let s' = rna_to_string l in
  Printf.printf "     rna list+tostring: \027[35m%s\027[0m\n%!" s';
  let l'' = generate_bases_triplets l in
  let s'' = triplet_to_string l'' in
  Printf.printf "generate_trip+tostring: \027[35m%s\027[0m\n%!" s'';
  let l''' = decode_arn l in
  let s''' = string_of_protein l''' in
  Printf.printf "   decode_arn+tostring: \027[35m%s\027[0m\n%!" s''';
  Printf.printf "\n%!"
  
				
let () =
  test (-1);
  test 0;
  test 1;
  test 2;
  test 3;
  test 4;
  test 5;
  test 7;
  test2 [U; A; A];
  test2 [U; A; A; A];
  test2 [U; A; A; A; A; A];
  test2 [G; C; A; G; C; C; G; C; G; G; C; U];
  test2 [G; C; A; G; C; C; G; C; G; G; C; U; U; A; A];
