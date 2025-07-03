open Encodage
open Chaines

(* Saisie des mots en mode T9 *)

(* 4 Avec saisie intuitive *)

(* Exercice 3 : Mots → touches *)

(*
  encoder_lettre : encodage -> char -> int
  Indique la touche numérique associée à une lettre donnée, selon un encodage T9.
  Paramètre code : liste associative (touche, liste de lettres associées),
                   représentant l'encodage du clavier T9.
  Paramètre lettre : caractère à encoder.
  Résultat : retourne le numéro de touche (int) associé à la lettre.
  Pré-conditions :
    - La lettre doit appartenir à l’une des listes associées dans le code.
    - L’encodage ne doit pas être vide.
  Post-conditions :
    - Retourne la première touche trouvée dans l'encodage contenant la lettre.
    - Si la lettre n'est présente dans aucune liste, une exception est levée (failwith ici).
  Exceptions :
    - Lève 'Failure "Liste encodage incorrecte"' si la lettre n'est trouvée dans aucune association.
*)

let rec encoder_lettre code lettre =
  match code with
  |[] -> failwith "Liste encodage incorrecte"
  |(touche, liste_lettres)::reste_code ->
    if List.mem lettre liste_lettres then touche
    else encoder_lettre reste_code lettre

let%test _ = encoder_lettre t9_map 'a' = 2
let%test _ = encoder_lettre t9_map 'b' = 2
let%test _ = encoder_lettre stupide_map 'a' = 2
let%test _ = encoder_lettre stupide_map 'b' = 3

let%test _ = try let _ = encoder_lettre t9_map '%' in false with Failure "Liste encodage incorrecte" -> true
let%test _ = try let _ = encoder_lettre [] 'a' in false with Failure "Liste encodage incorrecte" -> true
let%test _ = try let _ = encoder_lettre [(2, ['b'; 'c'])] 'a' in false with Failure "Liste encodage incorrecte" -> true

(*
  encoder_mot : encodage -> string -> int list
  Calcule la séquence de touches à presser pour écrire un mot en mode T9.
  Paramètre code : encodage, une liste associative (touche, liste de lettres associées)
  Paramètre mot : chaîne de caractères à encoder
  Résultat : une liste d'entiers représentant les touches à presser (une par lettre du mot)
  Pré-conditions :
    - Chaque lettre du mot doit être présente dans l'encodage.
    - L'encodage ne doit pas être vide.
  Post-conditions :
    - La longueur de la liste retournée est égale à celle du mot.
    - Chaque lettre est transformée en un seul chiffre correspondant à sa touche.
    - L'ordre des lettres est conservé.
  Exceptions :
    - Lève 'Failure "Liste encodage incorrecte"' si une lettre du mot n’est pas présente dans l’encodage
      (via 'encoder_lettre')
*)

let encoder_mot code mot =
  let liste_lettres = List.of_seq (String.to_seq mot) in
  List.map (encoder_lettre code) liste_lettres

let%test _ = encoder_mot t9_map "bonjour" = [2; 6; 6; 5; 6; 8; 7]
let%test _ = encoder_mot stupide_map "abc" = [2; 3; 3]
let%test _ = encoder_mot [(2,['a';'b';'c']) ; (3,['d';'e';'f'])] "def" = [3; 3; 3]

let%test _ = try let _ = encoder_mot t9_map "abc%" in false with Failure "Liste encodage incorrecte" -> true
let%test _ = try let _ = encoder_mot [] "abc" in false with Failure "Liste encodage incorrecte" -> true
let%test _ = try let _ = encoder_mot [(2, ['b'; 'c'])] "abc" in false with Failure "Liste encodage incorrecte" -> true



(*-----------------------------------------------------------------------------------------------*)



(* ▷ Exercice 4 Manipulation d’un dictionnaire *)

(* Definition du type dictionaire *)
type dico = Noeud of ( string list * (int * dico) list )

(* Créer un dictionnaire vide *)
let empty = Noeud([], [])

(*
  recherche : int -> (int * dico) list -> dico option
  Recherche un sous-dictionnaire associé à une touche donnée.
  Paramètre touche : la touche à rechercher.
  Paramètre liste_touche_dico : liste de couples (touche, sous-dictionnaire).
  Résultat : 
    - Some dico si la touche est trouvée dans la liste.
    - None si la touche est absente.
  Post-conditions : 
    - Le résultat est 'Some dico' ssi il existe un couple (touche, dico) dans la liste.
*)

let rec recherche touche liste_touche_dico =
  match liste_touche_dico with
  | [] -> None
  | (t_touche, t_dico)::q -> if (t_touche=touche) then Some t_dico
                             else recherche touche q

let%test _ = recherche 2 [(2, Noeud([], [])); (3, Noeud([], []))] = Some (Noeud([], []))
let%test _ = recherche 3 [(2, Noeud([], [])); (3, Noeud([], []))] = Some (Noeud([], []))
let%test _ = recherche 4 [(2, Noeud([], [])); (3, Noeud([], []))] = None
let%test _ = recherche 2 [] = None

(*
  maj : int -> dico -> (int * dico) list -> (int * dico) list
  Met à jour ou ajoute un couple (touche, sous-dictionnaire) dans une liste de couples.
  Paramètre touche : la touche à insérer ou mettre à jour.
  Paramètre nouveau_noeud : le dictionnaire associé à cette touche.
  Paramètre liste_touche_dico : liste d’associations (touche, sous-dictionnaire).
  Résultat :
    - Une nouvelle liste avec la touche mise à jour ou ajoutée en tête.
  Post-conditions :
    - Si la touche était déjà présente, son dictionnaire est remplacé par 'nouveau_noeud'.
    - Sinon, le couple (touche, nouveau_noeud) est ajouté en tête de la liste.
*)

let rec maj touche nouveau_noeud liste_touche_dico =
  match liste_touche_dico with
  | [] -> [(touche, nouveau_noeud)]
  | (t_touche, t_dico)::q -> if (t_touche =  touche) then (touche, nouveau_noeud)::q
                             else (t_touche, t_dico)::(maj touche nouveau_noeud q)

let%test _ = maj 2 (Noeud([], [])) [(2, Noeud(["a"], [])); (3, Noeud(["b"], []))] = [(2, Noeud([], [])); (3, Noeud(["b"], []))]
let%test _ = maj 3 (Noeud(["c"], [])) [(2, Noeud(["a"], [])); (3, Noeud(["b"], []))] = [(2, Noeud(["a"], [])); (3, Noeud(["c"], []))]
let%test _ = maj 2 (Noeud(["a"], [])) [] = [(2, Noeud(["a"], []))]

(*
  ajouter_mot : string -> string list -> string list
  Ajoute un mot à une liste s’il n’y est pas déjà.
  Paramètre mot : le mot à ajouter.
  Paramètre liste_mot : liste de mots à compléter.
  Résultat :
    - Une liste contenant le mot, sans doublons.
  Post-conditions :
    - Le mot est dans la liste résultante.
    - Si le mot était déjà présent, la liste est inchangée.
*)

let ajouter_mot mot liste_mot =
  if List.mem mot liste_mot then liste_mot
  else mot::liste_mot

let%test _ = ajouter_mot "a" [] = ["a"]
let%test _ = ajouter_mot "a" ["b"; "c"] = ["a"; "b"; "c"]
let%test _ = ajouter_mot "a" ["a"; "b"; "c"] = ["a"; "b"; "c"]

(*
  ajouter : encodage -> dico -> string -> dico
  Ajoute un mot à un dictionnaire T9.
  Paramètre encodage : liste associative (touche, lettres associées).
  Paramètre dico : dictionnaire T9 sous forme d’arbre (Noeud).
  Paramètre mot : mot à insérer dans le dictionnaire.
  Résultat :
    - Un nouveau dictionnaire contenant le mot.
  Pré-conditions :
    - Le mot est constitué uniquement de lettres présentes dans l'encodage.
    - Le dictionnaire est bien formé (chaîne valide de noeuds Noeud).
  Post-conditions :
    - Le mot apparaît bien dans un noeud à l’issue du chemin correspondant à sa suite de touches.
    - Les autres mots du dictionnaire sont conservés inchangés.
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

(*
  creer_dico : encodage -> string -> dico
  Construit un dictionnaire à partir d’un fichier de mots et d’un encodage.
  Paramètre encodage : liste associative (touche, lettres associées).
  Paramètre filename : nom ou chemin du fichier texte contenant un mot par ligne.
  Résultat :
    - Un dictionnaire ('dico') contenant tous les mots du fichier, encodés selon les touches.
  Pré-conditions :
    - Le fichier doit exister et être lisible.
    - Chaque ligne du fichier contient un mot valide (lettres uniquement).
    - Toutes les lettres des mots doivent être couvertes par l’encodage fourni.
  Post-conditions :
    - Tous les mots du fichier sont ajoutés au dictionnaire, selon les touches de leur encodage.
  Exceptions :
    - Peut échouer avec une exception système si le fichier est introuvable ou non lisible.
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

(*
  appartient : encodage -> dico -> string -> bool
  Vérifie si un mot est présent dans un dictionnaire T9.
  Paramètre encodage : liste associative (touche, lettres associées)
  Paramètre dico : dictionnaire T9 sous forme d'arbre (type dico)
  Paramètre mot : le mot à rechercher dans le dictionnaire
  Résultat :
    - true si le mot est présent dans le dictionnaire
    - false sinon
  Pré-conditions :
    - Le mot ne contient que des lettres présentes dans l’encodage
    - Le dictionnaire est bien formé (type Noeud)
  Post-conditions :
    - La fonction suit la suite de touches du mot, descend récursivement l’arbre
    - Si le chemin est valide et que le mot est dans le nœud terminal, retourne true
    - Si une touche du chemin n’est pas présente ou que le mot n’est pas dans le nœud terminal, retourne false
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

(*
  supprimer_dans_dictio : dico -> string -> int list -> dico
  Supprime un mot dans un dictionnaire en suivant la séquence de touches correspondante.
  Paramètre Noeud (liste_mots, liste_touches_dictio) : le dictionnaire courant à traiter
  Paramètre mot : le mot à supprimer
  Paramètre liste_touches : la suite des touches correspondant au mot à supprimer
  Résultat :
    - Un dictionnaire où le mot a été supprimé s’il était présent
  Post-conditions :
    - Si le mot était présent à la position finale, il est supprimé de la liste de mots
    - Toute branche devenant vide (aucun mot et aucun sous-dictionnaire) est supprimée (élagage)
    - Le reste du dictionnaire est inchangé
*)

let rec supprimer_dans_dictio (Noeud (liste_mots, liste_touches_dictio)) mot liste_touches =
  match liste_touches with
  | [] ->
      let liste_mots_maj = List.filter (fun m -> m <> mot) liste_mots in
      Noeud(liste_mots_maj, liste_touches_dictio)

  | touche::reste_touches ->
      match recherche touche liste_touches_dictio with
      | None -> 
          Noeud(liste_mots, liste_touches_dictio)
      | Some sous_dictio ->
          let sous_dictio_maj = supprimer_dans_dictio sous_dictio mot reste_touches in
          let liste_touches_dictio_maj = maj touche sous_dictio_maj liste_touches_dictio in
          let liste_touches_dictio_elaguee = (* filtrer pour élaguer (cf post-conditions) *)
              List.filter (fun (_, Noeud (mots, sous_dictios)) -> mots <> [] || sous_dictios <> []) liste_touches_dictio_maj
          in Noeud(liste_mots, liste_touches_dictio_elaguee)

(*
  supprimer : encodage -> dico -> string -> dico
  Supprime un mot dans un dictionnaire T9 à partir de son encodage.
  Paramètre code : encodage, liste (touche, lettres associées)
  Paramètre dictio : dictionnaire T9 dans lequel on veut supprimer un mot
  Paramètre mot : le mot à supprimer
  Résultat :
    - Un dictionnaire dans lequel le mot a été supprimé, si le mot était présent
  Pré-conditions :
    - Le mot ne doit contenir que des lettres présentes dans l’encodage
  Post-conditions :
    - Le mot est supprimé uniquement s’il est présent
    - Les branches vides (mots et sous-dictionnaires vides) sont supprimées du dictionnaire
*)

let supprimer code dictio mot =
  let liste_touches = encoder_mot code mot in
  supprimer_dans_dictio dictio mot liste_touches

let dico_test_a = Noeud ([], [(2, Noeud (["a"], []))])
let dico_test_a_ad = Noeud ([], [(2, Noeud (["a"], [(3, Noeud (["ad"], []))]))])
let dico_test_a_ad_at = Noeud ([], [(2, Noeud (["a"], [(3, Noeud (["ad"], [])); (8, Noeud (["at"], []))]))])

let dico_test_a_ad_at_lol = ajouter t9_map dico_test_a_ad_at "lol"
let dico_test_a_ad_at_lol_lo = ajouter t9_map dico_test_a_ad_at_lol "lo"

let%test _ = supprimer t9_map dico_test_a "a" = empty
let%test _ = supprimer t9_map dico_test_a_ad "ad" = dico_test_a
let%test _ = supprimer t9_map dico_test_a_ad_at_lol_lo "lo" = dico_test_a_ad_at_lol
let%test _ = supprimer t9_map dico_test_a_ad_at_lol "lol" = dico_test_a_ad_at
let%test _ = supprimer t9_map dico_test_a "nimporte" = dico_test_a

let%test _ = supprimer t9_map (ajouter t9_map empty "mael") "mael" = empty
let%test _ = appartient t9_map (supprimer t9_map dico_fr_t9 "abricot") "abricot" = false

(*
  coherent : encodage -> dico -> bool
  Vérifie si un dictionnaire est cohérent pour un encodage donné.

  Paramètre encodage : liste associative représentant le clavier (touche, lettres associées)
  Paramètre dico : dictionnaire T9 (type dico)

  Résultat :
    - true si le dictionnaire est cohérent (chaque mot correspond à son chemin)
    - false sinon

  Définition de la cohérence :
    - Un mot stocké dans un nœud doit être encodable via 'encoder_mot', 
      et le résultat doit être égal à la séquence de touches 
      correspondant au chemin depuis la racine jusqu’à ce nœud.

  Post-conditions :
    - Tous les mots de chaque nœud respectent le chemin de touches menant à ce nœud
    - Si un seul mot est mal placé, la fonction retourne 'false'

  Implémentation :
    - Un accumulateur 'parcouru' garde la trace du chemin depuis la racine
    - On vérifie que pour chaque mot, 'List.rev (encoder_mot encodage mot) = parcouru'
    - On parcourt récursivement tous les sous-dictionnaires avec mise à jour du chemin
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



(*-----------------------------------------------------------------------------------------------*)



(* Exercice 5 Récupération d’informations sur un dictionnaire *)

(*
  decoder_mot : dico -> int list -> string list
  Identifie l’ensemble des mots correspondant exactement à une séquence de touches dans un dictionnaire.

  Paramètre Noeud(liste_mot, liste_touche_dico) :
    - dictionnaire T9 représenté comme un arbre n-aire,
    - 'liste_mot' : liste des mots associés au nœud courant,
    - 'liste_touche_dico' : sous-dictionnaires indexés par touches.

  Paramètre list_touche :
    - liste d'entiers représentant la suite de touches tapée par l'utilisateur.

  Résultat :
    - liste des mots qui correspondent exactement à la séquence de touches donnée,
      selon la structure du dictionnaire.

  Pré-conditions :
    - Chaque touche de 'list_touche' est censée exister dans l'encodage utilisé pour construire le dictionnaire.
    - Le dictionnaire a été construit via des appels cohérents à 'ajouter'.

  Post-conditions :
    - Si un chemin complet est trouvé dans le dictionnaire, on retourne les mots associés au nœud terminal.
    - Si le chemin est interrompu (aucune branche correspondante), on retourne une liste vide.
    - Aucun mot partiel n’est retourné (il doit y avoir correspondance exacte avec le chemin).
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

(*
  matching_dictio : dico -> int list -> dico
  Parcours le dictionnaire jusqu’au nœud correspondant à une séquence de touches donnée.
  Paramètre Noeud(liste_mots, liste_touches_dictio) : dictionnaire T9 sous forme d’arbre.
  Paramètre liste_touches : liste d'entiers représentant un début de séquence de touches.
  Résultat :
    - Le sous-dictionnaire atteint en suivant la suite de touches fournie.
    - Si la séquence ne correspond à aucun chemin valide, retourne un dictionnaire vide (Noeud ([], [])).
  Post-conditions :
    - Le dictionnaire retourné peut être exploré.
*)

let rec matching_dictio (Noeud (liste_mots, liste_touches_dictio)) liste_touches =
  match liste_touches with
  | [] -> Noeud (liste_mots, liste_touches_dictio)
  | touche::reste_touches ->
      match recherche touche liste_touches_dictio with
      | None -> Noeud ([], [])
      | Some sous_dictio -> matching_dictio sous_dictio reste_touches

(*
  collecter : dico -> string list
  Récupère tous les mots contenus dans un dictionnaire (parcours total).
  Paramètre Noeud(liste_mots, liste_touches_dictio) : dictionnaire T9.
  Résultat :
    - Une liste contenant tous les mots des nœuds de l’arbre.
  Post-conditions :
    - L’ordre des mots n’est pas garanti.
    - Tous les mots présents dans le dictionnaire sont inclus dans le résultat.
    - Ne contient pas de doublons si le dictionnaire est bien construit.
*)

let rec collecter (Noeud (liste_mots, liste_touches_dictio)) =
  let mots = List.map (fun (_, sous_dictio) -> collecter sous_dictio) liste_touches_dictio
  in liste_mots @ List.flatten(mots)

(*
  prefixe : dico -> int list -> string list
  Retourne tous les mots d’un dictionnaire dont la séquence de touches commence par un certain préfixe.
  Paramètre dictio : dictionnaire T9.
  Paramètre list_touches : séquence de touches partiellement saisie par l'utilisateur.
  Résultat :
    - Une liste de mots dont le code T9 commence par cette séquence de touches.
  Pré-conditions :
    - La liste de touches peut être vide (dans ce cas, retourne tous les mots).
    - Le dictionnaire doit être bien formé.
  Post-conditions :
    - Si la séquence ne mène à aucun nœud, retourne une liste vide.
*)

let prefixe dictio list_touches =
  let sous_dictio = matching_dictio dictio list_touches in
  collecter sous_dictio

(*
  test.txt contient les mots suivants :
  - bon
  - bonne
  - bonjour
  - tendre
  - vendre
*)
let egalite_sans_ordre l1 l2 = (List.sort compare l1 = List.sort compare l2)

let%test _ = egalite_sans_ordre (prefixe dico_test_t9 [2;6]) ["bon"; "bonne"; "bonjour"]
let%test _ = egalite_sans_ordre (prefixe dico_test_t9 [8;3;6]) ["tendre"; "vendre"]

let%test _ = egalite_sans_ordre (prefixe dico_test_t9 []) ["bon"; "bonne"; "bonjour"; "tendre"; "vendre"]
let%test _ = egalite_sans_ordre (prefixe dico_test_t9 [1;2]) []

(*
  max_mots_code_identique : dico -> int
  Calcule le nombre maximal de mots associés à une même séquence de touches dans un dictionnaire.
  Paramètre dictio : dictionnaire T9 sous forme d’arbre (type dico).
  Résultat :
    - Un entier représentant le nombre maximum de mots présents dans un même nœud du dictionnaire.
  Post-conditions :
    - Le résultat est >= 0.
    - Si le dictionnaire est vide ('Noeud ([], [])'), retourne 0.
    - Sinon, retourne le maximum entre :
      - le nombre de mots dans le nœud courant,
      - les maximums des sous-dictionnaires récursivement.
  
  Interprétation :
  - Chaque nœud correspond à une séquence de touches.
  - Certains mots peuvent partager une même séquence (homonymes en T9).
  - Cette fonction retourne le nombre maximal de tels mots partageant une même séquence.
*)

let rec max_mots_code_identique dictio =
  match dictio with | Noeud (liste_mots, suites) ->
    let nb_mots = List.length liste_mots in
    let fct_max_pour_suite (code, sous_dictio) = max_mots_code_identique sous_dictio in
    let max_parmis_suites = List.map fct_max_pour_suite suites in
    let max_suites = List.fold_left max 0 max_parmis_suites in
    max nb_mots max_suites

let%test _ = max_mots_code_identique empty = 0
let%test _ = max_mots_code_identique (ajouter t9_map empty "bonjour") = 1

let%test _ = max_mots_code_identique dico_test_a = 1
let dico_test_a_ad_at_au = ajouter t9_map dico_test_a_ad_at "au"
let dico_test_a_ad_at_au = Noeud ([], [(2, Noeud (["a"], [(3, Noeud (["ad"], [])); (8, Noeud (["at"; "au"], []))]))])
let%test _ = max_mots_code_identique dico_test_a_ad_at_au = 2

(*
  lister : dico -> string list
  Retourne la liste de tous les mots présents dans un dictionnaire T9.
  Paramètre :
    - Noeud(liste_mot, liste_touche_dico) : dictionnaire à parcourir.
      - liste_mot : mots au nœud courant.
      - liste_touche_dico : liste de paires (touche, sous-dictionnaire).
  Résultat :
    - Une liste contenant tous les mots du dictionnaire, récursivement.
  Post-conditions :
    - L’ordre n’est pas garanti.
    - Tous les mots sont inclus sans doublon (si le dictionnaire est bien construit).
*)

let rec lister (Noeud (liste_mot, liste_touche_dico)) =
  match liste_mot with
    | mot::q_mot -> mot::(lister (Noeud (q_mot, liste_touche_dico)))
    | [] -> match liste_touche_dico with
      | [] -> []
      | (_, dico_t)::q_touche_dico -> (lister dico_t)@(lister (Noeud (liste_mot, q_touche_dico)))

let dico_test_list = lister dico_test_t9

let%test _ = List.mem "bon" dico_test_list
let%test _ = List.mem "bonne" dico_test_list
let%test _ = List.mem "bonjour" dico_test_list
let%test _ = List.mem "vendre" dico_test_list
let%test _ = List.mem "tendre" dico_test_list 
let%test _ = List.length dico_test_list = 5

let dico_test_list_stp = lister dico_test_stp

let%test _ = List.mem "bon" dico_test_list_stp
let%test _ = List.mem "bonne" dico_test_list_stp
let%test _ = List.mem "bonjour" dico_test_list_stp
let%test _ = List.mem "vendre" dico_test_list_stp
let%test _ = List.mem "tendre" dico_test_list_stp
let%test _ = List.length dico_test_list_stp = 5

(*
  proche : dico -> int list -> int -> string list
  Identifie les mots d’un dictionnaire correspondant à une séquence de touches erronée.

  Paramètres :
    - Noeud(liste_mot, liste_touche_dico) : le dictionnaire T9 (arbre).
    - liste_touche : la séquence tapée par l’utilisateur (peut contenir des erreurs).
    - n : nombre exact d’erreurs autorisées (différences de touches).

  Résultat :
    - Une liste de mots du dictionnaire dont le chemin de touches correspond à
      la séquence donnée, à exactement 'n' erreurs près.

  Pré-conditions :
    - n >= 0.
    - Les touches de la liste sont des entiers simulant des appuis valides.
    - Le dictionnaire est bien construit.

  Post-conditions :
    - Tous les mots retournés sont présents dans le dictionnaire.
    - Tous les mots ont une longueur de touches égale à celle de 'liste_touche'.
    - Chaque mot est accessible via un chemin dans l’arbre avec exactement 'n' divergences par rapport à 'liste_touche'.
*)

let rec proche (Noeud (liste_mot, liste_touche_dico)) liste_touche n =
  if n < 0 then []
  else 
    match liste_touche with
      | [] -> if n = 0 then liste_mot
              else []
      | next_touche::q_touche -> match liste_touche_dico with
        | [] -> []
        | (touche_t, dico_t)::q_touche_dico -> 
          if (touche_t=next_touche) then (proche dico_t q_touche n) @ (proche (Noeud (liste_mot, q_touche_dico)) liste_touche n)
          else (proche dico_t q_touche (n-1)) @ (proche (Noeud (liste_mot, q_touche_dico)) liste_touche n)

(*
  test2.txt contient les mots suivants :
  - bonbon
  - vendre
  - tendre
  - cheval
  - bon
  - ton
*)
let dico_test2_t9 = creer_dico t9_map "test2.txt"
let proche_bonbon5 = proche dico_test2_t9 (encoder_mot t9_map "bonbon") 5  

let%test _ = List.length proche_bonbon5 = 3
let%test _ = List.mem "tendre" proche_bonbon5
let%test _ = List.mem "vendre" proche_bonbon5
let%test _ = List.mem "cheval" proche_bonbon5
let%test _ = proche dico_test2_t9 (encoder_mot t9_map"cheval") 5 = ["bonbon"]
let%test _ = proche dico_test2_t9 (encoder_mot t9_map"vendre") 6 = ["cheval"]
let%test _ = proche dico_test2_t9 (encoder_mot t9_map "bon") 1 = ["ton"]
let%test _ = proche dico_test2_t9 (encoder_mot t9_map "ton") 1 = ["bon"]

let dico_test2_stp = creer_dico stupide_map "test2.txt"
let proche_bonbon2 = proche dico_test2_stp (encoder_mot stupide_map "bonbon") 2

let%test _ = List.length proche_bonbon5 = 3
let%test _ = List.mem "tendre" proche_bonbon2
let%test _ = List.mem "vendre" proche_bonbon2
let%test _ = List.mem "cheval" proche_bonbon2
