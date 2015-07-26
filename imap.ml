(* ici la clé de la pmap est un int *)
type key = int;;

(* définition d'une map polymorphe *)
type 'a t =
  | EM
  | NM of 'a t * key * 'a * 'a t * int;;


(* map vide *)
let empty = EM;;


(* mem x m retourne true si il existe un binding de x dans m, false sinon *)
let rec mem x m =
  match m with
    | EM -> false
    | NM (l, k, _, r, _) -> let c = compare x k in
	if c = 0 then true
	else if c < 0 then mem x l
	else mem x r;;

(* height retourne la hauteur du noed de la map *)
let height  = function
  | EM -> 0
  | NM(_,_,_,_,h) -> h;;


(* node l k e r retourne le noeud de fils gauche l, de fils droit r, et de binding k e *)
let node l k e r =
  NM (l, k, e, r, 1 + max (height l) (height r));;

exception Les_maths_sont_inconsistantes;;


(* balance l k e r retourne la map équilibrée *)
(* les déséquilibres ne doivent pas être trop important, sinon l'équilibrage est impossible : l'exception Les_maths_sont_inconsistantes est levée *)
(* l'utilisation d'un smart constructor pour créer les maps empêche d'avoir un déséquilibre trop important *)
let balance l k e r =
  let hl = height l and hr = height r in
    (* cas où le déséquilibre est dans le fils gauche *)
    if hl > hr + 1
    then (match l with 
            |NM (ll, lkv, lev, lr, _) -> (* on regarde le fils gauche *)
               if (height ll >= height lr) (* cas où le déséquilibre est dans le fils gauche-gauche *)
               then node ll lkv lev (node lr k e r) (* rotation simple droite *)
               else (match lr with (* cas où le déséquilibre est dans le fils gauche-droit *)
                       | NM (lrl, lrkv, lrev, lrr, _) -> node (node ll lkv lev lrl) lrkv lrev (node lrr k e r) (* rotation gauche-droite *)
                       | _ -> raise Les_maths_sont_inconsistantes)
            | _ -> raise Les_maths_sont_inconsistantes)
      (* cas où le déséquilibre est dans le fils droit *)
    else if hr > hl + 1
    then (match r with
            |NM (rl, rkv, rev, rr, _) -> (* on regarde le fils droit *)
               if (height rr > height rl) (* cas où le déséquilibre est dans le fils droit-droit *)
               then node (node l k e rl) rkv rev rr (* rotation simple gauche *)
               else (match rl with (* cas où le déséquilibre est dans le fils droit-gauche *)
                       | NM (rll, rlkv, rlev, rlr, _) -> node (node l k e rll) rlkv rlev (node rlr rkv rev rr) (* rotation droite-gauche *)
                       | _ -> raise Les_maths_sont_inconsistantes)
            | _ -> raise Les_maths_sont_inconsistantes)
      (* pas de déséquilibre, on renvoit la map *)
    else node l k e r;;


(* smart constructor des maps *)
(* add k e a retourne la map a auquelle on a ajouté le binding k e *)
(* si le binding existe déjà, on le remplace par le nouveau *)
let rec add k e a =
  match a with
    | EM -> NM (EM, k, e, EM, 1)
    | NM (ls, x, ex, rs, h) -> let c = compare k x in
        if c = 0 then NM (ls, k, e, rs, h) (* cas où le binding existe déjà *)
        else if c < 0 then balance (add k e ls) x ex rs (*on ajoute à gauche tout en équilibrant avec balance *)
        else balance ls x ex (add k e rs);; (*on ajoute à droite tout en équilibrant avec balance *)


(* find x m retourne le binding associé à x dans m *)
(* lève l'exception Not_found si le binding n'est pas dans la map *)
let rec find x m =
  match m with
    | EM -> raise Not_found
    | NM (l, k, ek, r, _) -> let c = compare x k in
        if c = 0 then ek
        else if c < 0 then find x l
        else find x r;;


(* remmin retourne la map sans son plus petit binding *)
(* retourne l'exception Not_found si la map est vide *)	
let rec remmin = function
  | EM -> raise Not_found
  | NM (EM, _, _, r, _) -> r (* le plus petit est tout à gauche *)
  | NM (ls, x, ex, rs, h) -> balance (remmin ls) x ex rs;;


(* min retourne le binding sous forme d'un couple *)
(* retourne l'exception Not_found si la map est vide *)
let rec min = function
  | EM -> raise Not_found
  | NM (EM, x, ex, r, _) -> (x, ex)
  | NM (l, _, _, r, _) -> min l;;

(* remove x m retourne m sans le binding x *)
(* retourne l'exception Not_found si le binding n'existe pas dans la map *)
let rec remove x m =
  match m with
    | EM -> raise Not_found
    | NM (ls, k, e, rs, h) -> let c = compare x k in
	if c < 0 then balance (remove x ls) k e rs
	else if c > 0 then balance ls k e (remove x rs)
	else match rs with (* cas où l'on est sur le bon binding *)
	  | EM -> ls (* si le fils droit est vide, il suffit d'ajouter le fils gauche *)
	  | _ -> let (a, b) = min rs in (* sinon on ajoute le plus petit élément du fils droit à la racine, et on reconstruit la map *)
	      balance ls a b (remmin rs);;
