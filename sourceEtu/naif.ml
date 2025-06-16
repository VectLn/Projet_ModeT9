open Encodage
open Chaines

(* Exception correspondant à une saisie qui ne correspond à aucune lettre dans l'encodage utilisé.

Exemple : la saisie de 2 2 2 2 0 ne correspond à aucune lettre de t9_map

*)
exception LettreNonTrouve



(* Exception qui est levée quand il manque des zéros dans une suite de touches pour décrire un mot.

Exemple : la suite de touche 2 2 2 3 3 0 ou 2 2 2 0 3 3 ( la suite valide serait 2 2 2 0 3 3 0)
*)
exception PauseManquante

(* Exception qui est levée quand il y a deux zéros successifs dans une suite de touches pour décrire mot.

Exemple : la suite de touche 2 2 0 0
*)
exception DoublePause



(* Saisie des mots sans le mode T9 *)
(* Pour marquer le passage d'un caractère à un autre, on utilise la touche 0 *)


(*
Récuperer le k-ième élément d'une liste

Param:
  k: l'indice de l'élément à récuperer.
  l: la liste

Retourne: 
  Le k-ième élément.
Preconditions:
  La liste a au moins k elements
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
Identifie la lettre saisie à partir d’une touche et du nombre de fois qu’elle a été
pressée.

Param:
  encodage: liste associative, aux touches associe les lettres dans l'ordre.
  saisie: couple d'entier, le premier indique la touche saisie et le second le nombre
          de fois qu'elle a été saisie
Retourne:
  La lettre décodée.
Pre-condition: 
  o La touche (fst saisie) fait partie des touches utilisées par l'encodeur.
  o L'encodage n'est pas une liste vide
Post-condition: le nombre de fois que la touche est pressée n'est pas plus grand que le
  nombre de lettre associée à la touche.
Exception:
  Lève LettreNonTrouve si la saisie ne correspond à aucune lettre dans encodage.

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
Identifie le mot saisi à partir d’une suite de touches.

Param:
  encodage: liste associative, aux touches associe les lettres dans l'ordre.
  touches: suite de touches saisie à décoder
Retourne:
  Le mot décodée.
Pre-condition: 
  o La suite de touche saisie est correspond à des lettres de l'encodage.
  o L'encodage n'est pas une liste vide.
  o La suite de touche saisie finie par un zéro.
Post-condition: 
  o Le nombre de fois qu'une touche est pressée n'est pas plus grand que le
  nombre de lettre associée à la touche (si pas respecté lève LettreNonTrouve)
  o Il y a des zéros entre deux suites de touches differentes et à la fin (si pas
  respecté lève PauseManquante).


Détails d'implémentation:
    On utilise un fold left pour prendre les touches saisies dans l'ordre et construire
    le mot au fur est à mesure. 
    Pour bien fonctionner nous devons mettre dans l'accumulateur les informations necessaire
    pour construire le mot.
    Nous avons choisi un accumulateur avec le type : (int * int ) option * string
      o Le (int * int) option peut être vu comme un contexte, le premier int correspond à la touche 
        qui est en train d'être répétée et le second au nombre de fois qu'on l'a répétée. 
      o Le string et le mot qui a été construit pour le moment.


*)
let decoder_mot encodage suite = 
  let aggreger acc el =
    match fst acc with
      |None -> if (el = 0) then raise DoublePause
        else (Some (el, 1), (snd acc))
      |Some (touche, nb) -> if (el = 0) then (None, (String.cat (snd acc) (String.make 1 (decoder_lettre encodage (touche, nb)))))
        else if (el = touche) then (Some (el, nb + 1), (snd acc))
        else raise PauseManquante
    in let res = List.fold_left aggreger (None, "") suite
      in match fst res with
        |None -> snd res
        |Some _ -> raise PauseManquante


    



let%test _ = decoder_mot t9_map [2; 0;] = "a"
let%test _ = decoder_mot t9_map [2; 2; 2; 0;] = "c"
let%test _ = decoder_mot t9_map [2; 2; 0;] = "b"
let%test _ = decoder_mot t9_map [2; 2; 0; 6; 6; 6; 0; 6; 6; 0; 5; 0; 6; 6; 6; 0; 8; 8; 0; 7; 7; 7; 0] = "bonjour"
let%test _ = decoder_mot t9_map [8; 8; 8; 0; 4; 4; 4; 0; 2; 2; 2; 0; 8; 0; 6; 6; 6; 0; 7; 7; 7; 0] = "victor"
let%test _ = try decoder_mot t9_map [2; 2; 2; 2; 0;] = "*" with LettreNonTrouve -> true;;
let%test _ = try decoder_mot stupide_map [1; 1; 0; 3; 0;] = "*" with LettreNonTrouve -> true;;
let%test _ = try decoder_mot t9_map [2; 2; 3; 0;] = "*" with PauseManquante -> true;;
let%test _ = try decoder_mot t9_map [2; 2; 0; 3;] = "*" with PauseManquante -> true;;

