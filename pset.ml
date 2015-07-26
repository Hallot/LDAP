(* avl polymorphique défini de manière récursive *)
type 'a t =
  |E
  |N of 'a t * 'a * 'a t * int;;


(* the empty set *)
let empty = E;;

(* singleton x builds a singleton containing element x *)
let singleton x = N(E, x, E, 1);;


(* mem n a retourne true si n appartient à a et false sinon *)
let rec mem n a =
  match a with
    | E -> false
    | N (fg,x,fd,_) -> let c = compare n x in
        (c = 0) ||
          if c < 0 then mem n fg
          else mem n fd;;


(* height retourne la hauteur de l'avl *)
let height  = function
  | E -> 0
  | N(_,_,_,h) -> h;;


(* node fg x fd retourne le noeud en calculant la hauteur *)
let node fg x fd =
  N(fg,x,fd,1 + max (height fg) (height fd));;

(* exceptions pour balance et add *)  
exception Les_maths_sont_inconsistantes;;
exception Already_there;;

(* balance l v r retourne l'arbre equilibré *)
(* les déséquilibres ne doivent pas être trop important, sinon l'équilibrage est impossible : l'exception Les_maths_sont_inconsistantes est levée *)
(* l'utilisation d'un smart constructor pour créer les avls empêche d'avoir un déséquilibre trop important *)
let balance l v r =
  let hl = height l and hr = height r in
    (* cas où le déséquilibre est dans le fils gauche *)
    if hl > hr + 1
    then (match l with
            |N (ll, lv, lr, _) -> (* on regarde le fils gauche *)
               if (height ll >= height lr) (* cas où le déséquilibre est dans le fils gauche-gauche *)
               then node ll lv (node lr v r) (* rotation simple droite *)
               else (match lr with (* cas où le déséquilibre est dans le fils gauche-droit *)
                       | N (lrl, lrv, lrr, _) -> node (node ll lv lrl) lrv (node lrr v r) (* rotation gauche-droite *)
                       | _ -> raise Les_maths_sont_inconsistantes)
            | _ -> raise Les_maths_sont_inconsistantes)
      (* cas où le déséquilibre est dans le fils droit *)
    else if hr > hl + 1
    then (match r with
            |N (rl, rv, rr, _) -> (* on regarde le fils droit *)
               if (height rr > height rl) (* cas où le déséquilibre est dans le fils droit-droit *)
               then node (node l v rl) rv rr (* rotation simple gauche *)
               else (match rl with (* cas où le déséquilibre est dans le fils droit-gauche *)
                       | N (rll, rlv, rlr, _) -> node (node l v rll) rlv (node rlr rv rr) (* rotation droite-gauche *)
                       | _ -> raise Les_maths_sont_inconsistantes)
            | _ -> raise Les_maths_sont_inconsistantes)
      (* pas de déséquilibre, on renvoit l'avl *)
    else node l v r;;


(* smart constructor *)
(* add e a retourne l'avl a auquel on a ajoute e en conservant l'invariant *)
let rec add_aux e a =
  match a with
    | E -> N (E,e,E,1)
    | N (ls,x,rs,_) -> let c = compare e x in
        if c < 0
        then balance (add_aux e ls) x rs
        else if c > 0
        then balance ls x (add_aux e rs)
        else raise Already_there;;

let add e a =
  try add_aux e a with
    |Already_there -> a;;


(* union a1 a2 retourne l'union des deux ensembles *)
let rec union a1 a2 =
  match a1 with
    | E -> a2
    | N(fg, x, fd, _) -> union fg (union fd (add x a2));;


(* inter a1 a2 retourne l'intersection de a1 et a2 *)
let rec inter_aux a1 a2 acc =
  match a1 with
    | E -> acc
    | N(fg, x, fd, _) -> if mem x a2 then inter_aux fg a2 (inter_aux fd a2 (add x acc))
      else inter_aux fg a2 (inter_aux fd a2 (acc));;

let inter a1 a2 = inter_aux a1 a2 E;;

(* inclus a1 a2 retourne true si a1 est inclu dans a2, false sinon *)
let rec inclus_aux a1 a2 acc =
  match a1 with
    | E -> acc
    | N(fg,x,fd,_) -> inclus_aux fg a2 (inclus_aux fd a2 (mem x a2));;


let inclus a1 a2 = inclus_aux a1 a2 true;;


(* remmin retourne l'avl sans son plus petit élément *)
(* retourne l'exception Not_found si l'avl est vide *)	
let rec remmin = function
  | E -> raise Not_found
  | N (E, _, r, _) -> r (* le plus petit est tout à gauche *)
  | N (ls, x, rs, h) -> balance (remmin ls) x rs;;


(* min retourne le plus petit élement de l'avl *)
(* retourne l'exception Not_found si l'avl est vide *)
let rec min = function
  | E -> raise Not_found
  | N (E, x, r, _) -> x
  | N (l, _, r, _) -> min l;;

(* remove x m retourne m sans l'élément x *)
(* retourne l'exception Not_found si l'élément x n'existe pas dans l'avl *)
let rec remove x m =
  match m with
    | E -> raise Not_found
    | N (ls, k, rs, h) -> let c = compare x k in
	if c < 0 then balance (remove x ls) k rs
	else if c > 0 then balance ls k (remove x rs)
	else match rs with (* cas où l'on est sur l'élément *)
	  | E -> ls (* si le fils droit est vide, il suffit de renvoyer le fils gauche *)
	  | _ -> balance ls (min rs) (remmin rs);; (* sinon on ajoute le plus petit élément du fils droit à la racine, et on reconstruit l'avl *)

