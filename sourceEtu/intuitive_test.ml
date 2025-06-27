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
  |[] -> failwith "Liste encodage incorrecte"
  |(touche, liste_lettres)::queue ->
    if List.mem lettre liste_lettres then touche
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
  let liste_lettres = List.of_seq (String.to_seq mot) in
  List.map (encoder_lettre code) liste_lettres

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


(* maj : int -> dico -> (int * dico) list -> (int * dico) list
    param touche       : int, touche à mettre à jour
    param nouveau_noeud: dico, sous-arbre mis à jour
    param liste_touche_dico: transitions initiales
    return nouvelle liste de transitions avec touche mise à jour ou ajoutée
*)
let rec maj touche nouveau_noeud liste_touche_dico =
  match liste_touche_dico with 
    |[] -> [(touche, nouveau_noeud)]
    |(t_touche, t_dico)::q -> if (t_touche =  touche) then (touche, nouveau_noeud)::q
                              else (t_touche, t_dico)::(maj touche nouveau_noeud q)



(* ajouter_mot : string -> string list -> string list
    param mot      : string, mot à ajouter
    param liste_mot: string list, liste initiale de mots
    return liste_mot avec mot ajouté si absent
*)
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

(* creer_dico : encodage -> string -> dico
    Construit un dictionnaire à partir d'un fichier de mots.
    param encodage: encodage, liste associative (touche * char list) list
    param filename : string, chemin vers le fichier (un mot par ligne)
    return dico, dictionnaire construit
    raise Sys_error si le fichier est inaccessible
*)

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

(* appartient : encodage -> dico -> string -> bool
    Vérifie si un mot existe dans le dictionnaire.
    param encodage: encodage, liste associative (touche * char list) list
    param dictio   : dico
    param mot      : string
    return true si mot présent, false sinon
*)

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

(* coherent : encodage -> dico -> bool
    Vérifie que tous les mots stockés sont accessibles par leur code.
    param encodage: encodage, liste associative
    param dictio   : dico
    return true si cohérent, false sinon
*)

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

(* decoder_mot : dico -> int list -> string list
    Retourne tous les mots correspondants exactement à la suite de touches.
    param dictio   : dico
    param code_seq : int list, suite de touches
    return string list, mots associés ou []
*)

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




















let rec supprimer_dans_dictio (Noeud (liste_mots, liste_touches_dictio)) mot liste_touches =
  match liste_touches with
  | [] ->
      let liste_mots_maj = List.filter (fun m -> m <> mot) liste_mots in
      Noeud(liste_mots_maj, liste_touches_dictio)
  | touche::reste_touches ->
      match recherche touche liste_touches_dictio with
      | None -> Noeud(liste_mots, liste_touches_dictio)
      | Some sous_dictio ->
          let sous_dictio_maj = supprimer_dans_dictio sous_dictio mot reste_touches in
          let liste_touches_dictio_maj = maj touche sous_dictio_maj liste_touches_dictio in
          let liste_touches_dictio_elaguee =
              List.filter (fun (_, Noeud (mots, sous_dictios)) -> mots <> [] || sous_dictios <> []) liste_touches_dictio_maj
          in Noeud(liste_mots, liste_touches_dictio_elaguee)


(* supprimer : encodage -> dico -> string -> dico
    Supprime un mot du dictionnaire et élagage des branches vides.
    @param encodage: encodage, liste associative
    @param dictio   : dico
    @param mot      : string à supprimer
    @return dico modifié
*)

let supprimer code dictio mot =
  let liste_touches = encoder_mot code mot in
  supprimer_dans_dictio dictio mot liste_touches




let%test _ = supprimer t9_map (ajouter t9_map empty "mael") "mael" = empty
let%test _ = appartient t9_map (supprimer t9_map dico_fr_t9 "abricot") "abricot" = false
let%test _ = appartient t9_map dico_fr_t9 "abricot" = true





(*
  Descend dans le dictionnaire selon une liste de touches.
  Retourne le sous-arbre correspondant au préfixe.
*)

(* matching_dictio : dico -> int list -> dico
    Descend dans le dictionnaire selon une liste de touches.
    @param dictio   : dico
    @param code_seq : int list
    @return sous-arbre correspondant ou Noeud ([], []) si aucun
*)
let rec matching_dictio (Noeud (liste_mots, liste_touches_dictio)) liste_touches =
  match liste_touches with
  | [] -> Noeud (liste_mots, liste_touches_dictio)
  | touche::reste_touches ->
      match recherche touche liste_touches_dictio with
      | None -> Noeud ([], [])
      | Some sous_dictio -> matching_dictio sous_dictio reste_touches

(*
  Récupère tous les mots dans un dictionnaire récursivement.
*)
let rec collecter (Noeud (liste_mots, liste_touches_dictio)) =
  let mots = List.flatten (List.map (fun (_, sous_dictio) -> collecter sous_dictio) liste_touches_dictio)
  in liste_mots @ mots


(*
  Liste l’ensemble des mots du dictionnaire correspondant à un préfixe de touches.
*)
(* prefixe : dico -> int list -> string list
    Liste tous les mots du dictionnaire dont le code commence par code_seq.
    @param dictio   : dico
    @param code_seq : int list, préfixe de touches
    @return mots correspondants (peut être vide)
*)
let prefixe dictio list_touches =
  let sous_dictio = matching_dictio dictio list_touches in
  collecter sous_dictio


let%test _ =
  List.sort compare (prefixe dico_test_t9 [2;6])
  = ["bon"; "bonne"; "bonjour"]
let%test _ =
  List.sort compare (prefixe dico_test_stp [3;2])
  = ["bon"; "bonne"; "bonjour"]





(* max_mots_code_identique : dico -> int
    Calcule le nombre maximal de mots partageant la même séquence de touches.
    @param dictio : dico
    @return int, maximum de |mots| parmi tous les nœuds
*)


let rec max_mots_code_identique dictio =
  match dictio with | Noeud (liste_mots, suites) ->
    let nb_mots = List.length liste_mots in
    let rec_max_pour_suite (code, sous_dictio) = max_mots_code_identique sous_dictio in
    let max_parmi_suites = List.map rec_max_pour_suite suites in
    let max_suites = List.fold_left max 0 max_parmi_suites in
    max nb_mots max_suites


(* Construction d’un dictionnaire de test *)
let dico_test =
  ajouter t9_map
    (ajouter t9_map
      (ajouter t9_map
        (ajouter t9_map empty "ad")
        "ae")
      "af")
    "bonjour"

let%test _ = max_mots_code_identique empty = 0
let%test _ = max_mots_code_identique (ajouter t9_map empty "salut") = 1
let%test _ = max_mots_code_identique dico_test = 3  (* "ad", "ae", "af" sur [2;3] *)

(* Dico avec collisions multiples *)
let dico2 =
  ajouter t9_map
    (ajouter t9_map
      (ajouter t9_map
        (ajouter t9_map
          (ajouter t9_map empty "ad")
          "ae")
        "af")
      "ag")
    "ah"

(* [ad, ae, af] -> [2;3], 3 mots
   [ag, ah]     -> [2;4], 2 mots *)

let%test _ = max_mots_code_identique dico2 = 3
