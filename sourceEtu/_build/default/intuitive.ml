open Encodage
open Chaines



(* Exercice 3 : Mots → touches *)

(* encoder_lettre : encodage -> char -> int
   Retourne la touche associée à la lettre passée en paramètre.
   Paramètre code : encodage, encodage utilisé.
   Paramètre lettre : char, lettre à coder.
   Résultat : numéro de la touche associée à la lettre. *)

let rec encoder_lettre code lettre =
  match code with
  |[] -> failwith "Erreur : caractère non reconnu"
  |(touche, lettres)::queue ->
    if List.mem lettre lettres then touche
    else encoder_lettre queue lettre

let%test _ = encoder_lettre t9_map 'a' = 2
let%test _ = encoder_lettre t9_map 'b' = 2
let%test _ = encoder_lettre stupide_map 'a' = 2
let%test _ = encoder_lettre stupide_map 'b' = 3

(* encoder_mot : encodage -> string -> int list
   Retourne la suite de touches pour le mot passé en paramètre.
   Paramètre code : encodage, encodage utilisé.
   Paramètre mot : string, mot à encoder.
   Résultat : liste d’entiers des touches pour taper le mot. *)

let encoder_mot code mot =
  List.map (encoder_lettre code) (List.of_seq (String.to_seq mot))

let%test _ = encoder_mot t9_map "bonjour" = [2; 6; 6; 5; 6; 8; 7]




(* Saisie des mots en mode T9 *)



(* Definition du type dictionaire*)
type dico = Noeud of ( string list * ( int * dico) list )


(*
Créer un dictionnaire vide
*)
let empty = Noeud([], [])



(*
Renvoyer le dictionnaire associée à la touche passée en paramètre parmis une liste de 
couples (touche, dictionnaire) passée en paramètre.
Param:
  touche: la touche cherchée
  list_touche_dico: liste de couples (touche, dictionnaire) correspondants à la suite
    du dictionnaire.
Retourne:
  Le dictionnaire avec le mot ajouté.

*)
let rec recherche touche liste_touche_dico =
  match liste_touche_dico with 
    |[] -> None
    |(t_touche, t_dico)::q -> if (t_touche=touche) then Some t_dico
                              else recherche touche q


(*

*)
let rec maj touche nouveau_noeud liste_touche_dico =
  match liste_touche_dico with 
    |[] -> [(touche, nouveau_noeud)]
    |(t_touche, t_dico)::q -> if (t_touche =  touche) then (touche, nouveau_noeud)::q
                              else (t_touche, t_dico)::(maj touche nouveau_noeud q)



let ajouter_mot mot liste_mot =
  if List.mem mot liste_mot then liste_mot
  else mot::liste_mot

(*
Ajoute un mot à un dictionnaire.

Param:
  encodage: liste associative, aux touches associe les lettres.
  Noeud(liste_mot, liste_dico) : dictionnaire auquel on veut ajouter le mot.
    liste_mot correspond à la liste de mots associée au premier noeud,
    list_touche_dico: liste de couples (touche, dictionnaire) correspondants à la suite
    du dictionnaire.
Retourne:
  Le dictionnaire avec le mot ajouté.

*)
let ajouter encodage (Noeud (liste_mot, liste_touche_dico)) mot = 

  let rec ajouter_liste_touche (Noeud (liste_mot, liste_touche_dico)) liste_touche =
    match liste_touche with
      |[] -> Noeud((ajouter_mot mot liste_mot), liste_touche_dico)
      |touche::q -> 
        let dico_r = 
          match (recherche touche liste_touche_dico) with
            |None -> empty
            |Some dico -> dico 
        in let nouveau_noeud = ajouter_liste_touche dico_r q 
        in Noeud(liste_mot, maj touche nouveau_noeud liste_touche_dico)

    in let liste_touche = (encoder_mot encodage mot) in ajouter_liste_touche (Noeud (liste_mot, liste_touche_dico)) liste_touche



let%test _ = ajouter t9_map empty "a" = Noeud([], [(2, Noeud(["a"], []))])
let%test _ = ajouter t9_map (Noeud([], [(2, Noeud(["a"], []))])) "a" = Noeud([], [(2, Noeud(["a"], []))])
let%test _ = ajouter t9_map (Noeud([], [(2, Noeud(["a"], []))])) "ad" = Noeud([], [(2, Noeud(["a"], [(3, Noeud(["ad"], []))]))])
let%test _ = ajouter t9_map (Noeud([], [(2, Noeud(["a"], [(3, Noeud(["ad"], []))]))])) "at" = Noeud([], [(2, Noeud(["a"], [(3, Noeud(["ad"], [])); (8, Noeud(["at"], []))]))])



let creer_dico encodage filename =
  let in_channel = open_in filename in
  let rec loop dico =
    try
      let mot = input_line in_channel in
      let nouveau_dico = ajouter encodage dico mot in
      loop nouveau_dico
    with
    | End_of_file ->
        close_in in_channel;
        dico
  in
  loop empty

let dico_fr_t9 = creer_dico t9_map "dico_fr.txt"
let dico_fr_stp = creer_dico stupide_map "dico_fr.txt"



let appartient encodage (Noeud (liste_mot, liste_touche_dico)) mot = 
  let rec appartient_liste_touche (Noeud (liste_mot, liste_touche_dico)) liste_touche = 
    match liste_touche with
      |[] -> List.mem mot liste_mot
      |touche::q -> match (recherche touche liste_touche_dico) with
        |None -> false
        |Some dico -> appartient_liste_touche dico q
  in appartient_liste_touche (Noeud (liste_mot, liste_touche_dico)) (encoder_mot encodage mot)


let%test _ = appartient t9_map dico_fr_t9 "abricot"
let%test _ = appartient t9_map dico_fr_t9 "coutelier"
let%test _ = appartient t9_map dico_fr_t9 "museographe"
let%test _ = appartient t9_map dico_fr_t9 "phosphoreux"
let%test _ = appartient t9_map dico_fr_t9 "reengageant"
let%test _ = appartient t9_map dico_fr_t9 "zygopetalum"

let%test _ = appartient t9_map dico_fr_t9 "anticonstitutionnellement"
let%test _ = appartient t9_map dico_fr_t9 "sourire"
let%test _ = appartient t9_map dico_fr_t9 "chat"
let%test _ = appartient t9_map dico_fr_t9 "chien"
let%test _ = appartient t9_map dico_fr_t9 "escargot"
let%test _ = appartient t9_map dico_fr_t9 "tennis"

let%test _ = appartient stupide_map dico_fr_stp "abricot"
let%test _ = appartient stupide_map dico_fr_stp "coutelier"
let%test _ = appartient stupide_map dico_fr_stp "museographe"
let%test _ = appartient stupide_map dico_fr_stp "phosphoreux"
let%test _ = appartient stupide_map dico_fr_stp "reengageant"
let%test _ = appartient stupide_map dico_fr_stp "zygopetalum"

let%test _ = appartient stupide_map dico_fr_stp "vague"
let%test _ = appartient stupide_map dico_fr_stp "mer"
let%test _ = appartient stupide_map dico_fr_stp "ocean"
let%test _ = appartient stupide_map dico_fr_stp "vacance"
let%test _ = appartient stupide_map dico_fr_stp "repos"
let%test _ = appartient stupide_map dico_fr_stp "sieste"

let%test _ = appartient t9_map dico_fr_t9 "abric" = false
let%test _ = appartient t9_map dico_fr_t9 "couteliera" = false
let%test _ = appartient t9_map dico_fr_t9 "ae" = false
let%test _ = appartient t9_map dico_fr_t9 "phosphoreu" = false
let%test _ = appartient t9_map dico_fr_t9 "reeageant" = false
let%test _ = appartient t9_map dico_fr_t9 "" = false

let%test _ = appartient stupide_map dico_fr_stp "erekrj" = false
let%test _ = appartient stupide_map dico_fr_stp "kjahoguif" = false
let%test _ = appartient stupide_map dico_fr_stp "iuegfzds" = false
let%test _ = appartient stupide_map dico_fr_stp "tuy" = false
let%test _ = appartient stupide_map dico_fr_stp "rayt" = false
let%test _ = appartient stupide_map dico_fr_stp "kjhjgh" = false



let coherent encodage (Noeud (liste_mot, liste_touche_dico)) =
  let rec coherent_parcouru (Noeud (liste_mot, liste_touche_dico)) parcouru =
    match liste_mot with 
      |mot::q_mot -> if (List.equal (=) (List.rev (encoder_mot encodage mot)) parcouru) then coherent_parcouru (Noeud (q_mot, liste_touche_dico)) parcouru
        else false
      |[] -> match liste_touche_dico with
        |[] -> true
        |(touche_t, dico_t)::q_touche_dico -> if coherent_parcouru dico_t (touche_t::parcouru) 
                                                then coherent_parcouru (Noeud ([], q_touche_dico)) parcouru
                                              else false

  in coherent_parcouru (Noeud (liste_mot, liste_touche_dico)) []

let%test _ = coherent t9_map dico_fr_t9
let%test _ = coherent stupide_map dico_fr_t9 = false
let%test _ = coherent t9_map dico_fr_stp = false
let%test _ = coherent stupide_map dico_fr_stp


let rec decoder_mot (Noeud (liste_mot, liste_touche_dico)) list_touche =
  match list_touche with
    |[] -> liste_mot
    |touche_t::touche_q -> match (recherche touche_t liste_touche_dico) with
        |None -> []
        |Some dico -> decoder_mot dico touche_q



let dico_test_t9 = creer_dico t9_map "test.txt"
let dico_test_stp = creer_dico stupide_map "test.txt"

let%test _ = decoder_mot dico_test_stp [3; 2; 3] = ["bon"]
let%test _ = decoder_mot dico_test_stp [3; 2; 3; 3; 3; 2] = ["tendre"; "vendre"]
let%test _ = decoder_mot dico_test_stp [3; 2; 3; 3; 2; 2; 3] = ["bonjour"]
let%test _ = decoder_mot dico_test_stp [3; 2; 3; 3; 2; 3] = []
let%test _ = decoder_mot dico_test_stp [3; 2; 3; 3] = []

let%test _ = decoder_mot dico_test_t9 [2; 6; 6; 5; 6; 8; 7] = ["bonjour"]
