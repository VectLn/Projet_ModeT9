open Encodage
open Chaines

(* Saisie des mots sans le mode T9 *)
(* Pour marquer le passage d'un caractère à un autre, on utilise la touche 0 *)

(* 3 Sans saisie intuitive *)

(* ▷ Exercice 1 Mots → touches *)

(*
  index : char -> char list -> int
  Renvoie la position de la lettre dans la liste.
  Paramètre lettre : caractère à chercher
  Paramètre liste_lettres : liste des lettres associées à une touche
  Résultat : un entier représentant la position de la lettre dans la liste (1 pour le premier, etc.)
  Pré-conditions : la lettre doit se trouver dans la liste (sinon exception)
  Post-condition : le résultat est strictement positif et correspond à la position de la première occurrence
  Exceptions :
    - lève l'exception Failure "Lettre incorrecte" si la lettre n'est pas trouvée
*)

let rec index lettre liste_lettres =
  match liste_lettres with
  | [] -> failwith "Lettre incorrecte"
  | tete::queue -> if tete = lettre then 1
                   else 1 + (index lettre queue)

let%test _ = index 'a' ['a';'b';'c'] = 1
let%test _ = index 'b' ['a';'b';'c'] = 2
let%test _ = index 'c' ['a';'b';'c'] = 3
(*let%test _ = try index 'd' ['a';'b';'c'] = 1 with Invalid_argument "Lettre incorrecte" -> true;;
let%test _ = try index 'a' [] = 1 with Invalid_argument "Lettre incorrecte" -> true;;*)

(*
  encoder_lettre : (int * char list) list -> char -> (int * int)
  Indique la touche et le nombre d’appuis nécessaires pour saisir une lettre.
  Paramètre code : encodage, une liste associative (touche, lettres associées)
  Paramètre lettre : caractère à encoder
  Résultat : un couple (numéro de touche, nombre d’appuis nécessaires)
  Pré-conditions : 
    - La lettre doit apparaître au moins une fois dans les listes de l'encodage
    - L'encodage doit être bien formé (pas de touches dupliquées inutilement)
  Post-condition : 
    - Le couple retourné correspond à la première association trouvée
    - Le nombre d’appuis est ≥ 1
  Exceptions :
    - lève Failure "Liste encodage incorrecte" si la lettre n’est dans aucune des sous-listes
    - lève Failure "Lettre incorrecte" si l’appel à index échoue
*)

let rec encoder_lettre code lettre =
  match code with
  | [] -> failwith "Liste encodage incorrecte"
  | (touche, liste_lettres)::queue ->
      if List.mem lettre liste_lettres then (touche, index lettre liste_lettres)
      else encoder_lettre queue lettre

let%test _ = encoder_lettre t9_map 'c' = (2,3)
let%test _ = encoder_lettre stupide_map 'e' = (2,2)
let%test _ = encoder_lettre [(2,['a';'b';'c'])] 'b' = (2,2)
let%test _ = encoder_lettre [(2,['a';'b';'c']) ; (3,['d';'e';'f'])] 'd' = (3,1)
(*let%test _ = try encoder_lettre t9_map '%' = (2,1) with Invalid_argument "Lettre incorrecte" -> true;;
let%test _ = try encoder_lettre [] 'a' = (2,1) with Invalid_argument "Liste encodage incorrecte" -> true;;*)

(*
  sequence_encoder_lettre : (int * char list) list -> char -> int list
  Encode une lettre sous forme d’une séquence de pressions de touches.
  Paramètre code : liste associative représentant l'encodage (touche, liste de lettres)
  Paramètre lettre : caractère à encoder
  Résultat : liste d’entiers représentant les pressions nécessaires, suivie de 0 (pause)
  Pré-conditions :
    - La lettre doit être présente dans le code
  Post-condition :
    - La longueur de la liste est égale au nombre d’appuis + 1 (pour la pause)
    - Tous les éléments sauf le dernier sont égaux à la touche correspondante
  Exceptions :
    - Propagation de toutes les exceptions levées par encoder_lettre (Failure ...)
*)

let sequence_encoder_lettre code lettre =
  let (touche, nb_repetitions) = encoder_lettre code lettre in
  let sequence = List.init nb_repetitions (fun _ -> touche) in
  sequence @ [0]

let%test _ = sequence_encoder_lettre t9_map 'c' = [2; 2; 2; 0]
let%test _ = sequence_encoder_lettre stupide_map 'e' = [2; 2; 0]
let%test _ = sequence_encoder_lettre [(2,['a';'b';'c'])] 'b' = [2; 2; 0]

(*
  encoder_mot : (int * char list) list -> string -> int list
  Encode un mot entier en séquence de pressions sur les touches.
  Paramètre code : encodage, une liste associative (touche, lettres associées)
  Paramètre mot : chaîne de caractères à encoder
  Résultat : liste d’entiers représentant la séquence complète de pressions avec les pauses
  Pré-conditions :
    - Tous les caractères du mot doivent être encodables selon le code donné
    - Le mot est une chaîne de lettres uniquement (pas de chiffres ou ponctuations)
  Post-condition :
    - La concaténation des séquences pour chaque lettre avec `0` en séparateur est respectée
  Exceptions :
    - lève Failure "Lettre incorrecte" ou "Liste encodage incorrecte" si une lettre ne peut pas être encodée
    - propagées depuis sequence_encoder_lettre
*)

let rec encoder_mot code mot =
  match mot with
  | "" -> []
  | _ -> let lettre = mot.[0] in
         let sous_mot = String.sub mot 1 (String.length mot - 1) in
         (sequence_encoder_lettre code lettre) @ (encoder_mot code sous_mot)

let%test _ = encoder_mot t9_map "bonjour" = [2;2;0;6;6;6;0;6;6;0;5;0;6;6;6;0;8;8;0;7;7;7;0]
let%test _ = encoder_mot stupide_map "abc" = [2; 0; 3; 0; 3; 3; 0]
let%test _ = encoder_mot [(2,['a';'b';'c']) ; (3,['d';'e';'f'])] "def" = [3; 0; 3; 3; 0; 3; 3; 3; 0]
(*let%test _ = try encoder_mot t9_map "abc" = [] with Invalid_argument "Mot incorrect" -> true;;
let%test _ = try encoder_mot t9_map "" = [] with Invalid_argument "Mot incorrect" -> true;;
let%test _ = try encoder_mot [] "abc" = [] with Invalid_argument "Liste encodage incorrecte" -> true;;*)



(*-----------------------------------------------------------------------------------------------*)



(*▷ Exercice 2 Touches → mot*)

(* 
  Exception correspondant à une saisie qui ne correspond à aucune lettre dans l'encodage utilisé.
  Exemple : la saisie de 2 2 2 2 0 ne correspond à aucune lettre de t9_map
*)

exception LettreNonTrouve

(* 
  Exception qui est levée quand il manque des zéros dans une suite de touches pour décrire un mot.
  Exemple : la suite de touche 2 2 2 3 3 0 ou 2 2 2 0 3 3 ( la suite valide serait 2 2 2 0 3 3 0)
*)

exception PauseManquante

(* 
  Exception qui est levée quand il y a deux zéros successifs dans une suite de touches pour décrire mot.
  Exemple : la suite de touche 2 2 0 0
*)

exception DoublePause

(*
  get_kth_element : int -> 'a list -> 'a
  Récupère le k-ième élément d'une liste.
  Paramètre k : indice de l’élément à récupérer (1 pour le premier)
  Paramètre l : liste dans laquelle on cherche l’élément
  Résultat : l’élément à la position k
  Pré-conditions : 
    - k ≥ 1
    - la liste contient au moins k éléments
  Post-condition : 
    - L’élément retourné est celui à la position k
  Exceptions :
    - Lève Not_found si la liste a moins de k éléments
*)

let rec get_kth_element k l = 
  match l with
  |[] -> raise Not_found
  |t::q -> if (k=1) then t else get_kth_element (k-1) q

let%test _ = get_kth_element 3 [11; -3; 10] = 10
let%test _ = get_kth_element 1 [11; -3; 10] = 11
let%test _ = get_kth_element 2 [11; -3; 10] = -3
let%test _ = try get_kth_element 4 [1; 2; 4] = -1 with Not_found -> true;;

(*
  decoder_lettre : (int * char list) list -> (int * int) -> char
  Identifie la lettre à partir d’une touche et du nombre de pressions.
  Paramètre encodage : liste associative (touche, liste de lettres associées)
  Paramètre saisie : couple (touche, nombre de pressions effectuées)
  Résultat : la lettre correspondante à cette saisie
  Pré-conditions :
    - La touche doit exister dans l’encodage
    - Le nombre de pressions est ≥ 1 et ≤ nombre de lettres associées à cette touche
    - L'encodage n’est pas vide
  Post-condition :
    - Retourne la lettre à la (snd saisie)-ième position dans la liste associée à la touche
  Exceptions :
    - Lève LettreNonTrouve si la touche est absente ou le nombre d’appuis dépasse la liste
*)

let rec decoder_lettre encodage saisie = 
  match encodage with
  |[] -> raise LettreNonTrouve
  |(touche, lettres)::q -> 
    if (fst saisie = touche) then 
      try get_kth_element (snd saisie) lettres
      with Not_found -> raise LettreNonTrouve
    else decoder_lettre q saisie

let%test _ = decoder_lettre t9_map (2, 1) = 'a'
let%test _ = decoder_lettre t9_map (3, 3) = 'f'
let%test _ = decoder_lettre t9_map (3, 1) = 'd'
let%test _ = decoder_lettre t9_map (7, 2) = 'q'
let%test _ = decoder_lettre t9_map (7, 4) = 's'
let%test _ = decoder_lettre t9_map (9, 4) = 'z'
let%test _ = decoder_lettre stupide_map (2, 3) = 'i'
let%test _ = decoder_lettre stupide_map (3, 10) = 'm'
let%test _ = try decoder_lettre t9_map (1, 4) = '*' with LettreNonTrouve -> true;;
let%test _ = try decoder_lettre t9_map (10, 3) = '*' with LettreNonTrouve -> true;;
let%test _ = try decoder_lettre stupide_map (2, 7) = '*' with LettreNonTrouve -> true;;
let%test _ = try decoder_lettre stupide_map (1, 1) = '*' with LettreNonTrouve -> true;;

(*
  decoder_mot : (int * char list) list -> int list -> string
  Décode un mot à partir d’une séquence de touches en mode non-intuitif.
  
  Paramètre encodage : liste associative représentant l’encodage du clavier, 
                       associant à chaque touche un ensemble ordonné de lettres.
  Paramètre suite : liste d’entiers représentant la séquence de touches appuyées, 
                    avec 0 comme séparateur entre les lettres.

  Résultat : chaîne représentant le mot décodé.

  Pré-conditions :
    - L'encodage est non vide.
    - La suite de touches suit strictement le protocole :
        * 0 sépare deux lettres différentes (ou une fin de mot)
        * Pas de double 0 (levée de l’exception DoublePause)
        * Une touche ne peut pas être suivie directement par une autre sans 0 (levée de l’exception PauseManquante)
        * Le nombre d'appuis ne dépasse pas la taille de la liste associée à cette touche (sinon LettreNonTrouve)

  Post-condition :
    - La chaîne retournée contient exactement une lettre pour chaque séquence de pressions valides
    - L'ordre est respecté

  Exceptions :
    - LettreNonTrouve : si une séquence dépasse le nombre de lettres disponibles pour la touche
    - PauseManquante : si deux touches différentes se succèdent sans 0 entre elles
    - DoublePause : si deux 0 se suivent
*)

let decoder_mot encodage suite = 
  let aggreger acc el =
    match fst acc with
      | None -> if (el = 0) then raise DoublePause
        else (Some (el, 1), (snd acc))
      | Some (touche, nb) -> if (el = 0) then (None, ((snd acc) ^ String.make 1 (decoder_lettre encodage (touche, nb)))
) (*(String.cat (snd acc) (String.make 1 (decoder_lettre encodage (touche, nb))))*)
        else if (el = touche) then (Some (el, nb + 1), (snd acc))
        else raise PauseManquante
    in let res = List.fold_left aggreger (None, "") suite
      in match fst res with
        | None -> snd res
        | Some _ -> raise PauseManquante

let%test _ = decoder_mot t9_map [2; 0;] = "a"
let%test _ = decoder_mot t9_map [2; 2; 2; 0;] = "c"
let%test _ = decoder_mot t9_map [2; 2; 0;] = "b"
let%test _ = decoder_mot t9_map [2; 2; 0; 6; 6; 6; 0; 6; 6; 0; 5; 0; 6; 6; 6; 0; 8; 8; 0; 7; 7; 7; 0] = "bonjour"
let%test _ = decoder_mot t9_map [8; 8; 8; 0; 4; 4; 4; 0; 2; 2; 2; 0; 8; 0; 6; 6; 6; 0; 7; 7; 7; 0] = "victor"
let%test _ = try decoder_mot t9_map [2; 2; 2; 2; 0;] = "*" with LettreNonTrouve -> true;;
let%test _ = try decoder_mot stupide_map [1; 1; 0; 3; 0;] = "*" with LettreNonTrouve -> true;;
let%test _ = try decoder_mot t9_map [2; 2; 3; 0;] = "*" with PauseManquante -> true;;
let%test _ = try decoder_mot t9_map [2; 2; 0; 3;] = "*" with PauseManquante -> true;;
