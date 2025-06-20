open Encodage
open Chaines







(* Saisie des mots sans le mode T9 *)
(* Pour marquer le passage d'un caractère à un autre, on utilise la touche 0 *)

(* 
  index : char -> char list -> int
   Fonction qui renvoie l'index du caractère c dans la liste liste.
   Paramètre c : char, le caractère à chercher.
   Paramètre liste_lettres : char list, la liste dans laquelle chercher le caractère.
   Résultat : un entier, l'index du caractère c dans la liste liste.
   Pré-condition : c est une lettre de l'alphabet latin et liste non vide
   Post-condition :
   Si c est dans liste_lettres, renvoie l'index de c dans liste_lettres (commençant à 1).
   Si c n'est pas dans liste_lettres, lève une exception avec le message "Lettre incorrecte".
*)

let rec index lettre liste_lettres =
  match liste_lettres with
  | [] -> failwith "Lettre incorrecte"
  | tete::queue -> if tete = lettre then 1
                   else 1 + index lettre queue

let%test _ = index 'a' ['a';'b';'c'] = 1
let%test _ = index 'b' ['a';'b';'c'] = 2
let%test _ = index 'c' ['a';'b';'c'] = 3
let%test _ = try index 'd' ['a';'b';'c'] = 1 with Invalid_argument "Lettre incorrecte" -> true;;
let%test _ = try index 'a' [] = 1 with Invalid_argument "Lettre incorrecte" -> true;;

(*
  EXERCICE 1
  encoder_lettre : encodage -> char -> (int * int)
  Indique la touche et le nombre de fois qu’il faut appuyer dessus 
  pour saisir la lettre passée en paramètre. 
  Paramètre encodage : liste associative d'encodage (touche, liste de lettres)
  Paramètre l : lettre
  Résultat : renvoie le couple (touche, nombre d'appuis) pour saisir la lettre donnée
  Pré-conditions : l une lettre de l'alphabet latine et liste non vide
  Post-condition : si l est dans la liste, renvoie le couple (touche, index de l dans la liste)
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
let%test _ = try encoder_lettre t9_map '%' = (2,1) with Invalid_argument "Lettre incorrecte" -> true;;
let%test _ = try encoder_lettre [] 'a' = (2,1) with Invalid_argument "Liste encodage incorrecte" -> true;;

(*
  EXERCICE 1
  encoder_mot : encodage -> string -> int list
  Calcule la suite de touches à presser pour saisir un mot passé en paramètre.
  Paramètre encodage : liste associative d'encodage (touche, liste de lettres)
  Paramètre mot : mot à encoder
  Résultat : une liste d'entiers représentant les touches à presser pour saisir le mot
  Pré-conditions : mot non vide et encodage non vide
  Post-condition : renvoie la liste des touches à presser pour saisir le mot
*)

let repeter touche nb_appuis =
  List.init nb_appuis (fun _ -> touche)

let%test _ = repeter 2 3 = [2;2;2]
let%test _ = repeter 5 0 = []

let encoder_mot code mot =
  let rec aux code mot liste =
    match mot with
    | "" -> liste
    | s -> let lettre = s.[0] in
           let (touche, nombre) = encoder_lettre code lettre in
           let ajout = List.init nombre (fun _ -> touche) in
           aux code (String.sub s 1 ((String.length s) - 1)) (liste @ ajout @ [0])
  in aux code mot []

let%test _ = encoder_mot t9_map "bonjour" = [2;2;0;6;6;6;0;6;6;0;5;0;6;6;6;0;8;8;0;7;7;7;0]
let%test _ = encoder_mot stupide_map "bonjour" = [2;2;0;3;3;3;0;3;3;0;2;0;3;3;3;0;8;8;0;7;7;7;0]
let%test _ = encoder_mot [(2,['a';'b';'c'])] "abc" = [2;2;2]
let%test _ = encoder_mot [(2,['a';'b';'c']) ; (3,['d';'e';'f'])] "def" = [3;3;3]
let%test _ = try encoder_mot t9_map "" = [] with Invalid_argument "Mot incorrect" -> true;;
let%test _ = try encoder_mot [] "abc" = [] with Invalid_argument "Liste encodage incorrecte" -> true;;












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

