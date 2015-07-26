type vtypes = 
  | IntV of int 
  | StringV of string;;


type record = (string * vtypes) list;;

module Srecord = struct
  type t = record
  let compare = Pervasives.compare
end
module Unique = Unique.UniqueMake(Srecord);;

type uset = Unique.t Pset.t;;


type itypes = 
  | IntT of uset Imap.t 
  | StringT of uset Smap.t;;


type stypes = 
  | Int 
  | String;;


type schema = (string * stypes) list;;


type ldap = itypes Smap.t;;

type querytype =
  | And of querytype * querytype
  | Or of querytype * querytype
  | Filter of (string * (itypes -> uset));;

exception SchemaError of string;;

(* fonction permettant de créer un identifiant identique quelque soit l'ordre de la liste *)
(* create : record -> Unique.v *)
let create x = Unique.create (List.sort Pervasives.compare x)

(*********************************************************************)								   
(*             fonction d'initialisation d'un ldap                   *)
(*********************************************************************)

(* init schem retourne un ldap vide sur la base du schema donné *)
let rec init schem =
  match schem with
    | [] -> Smap.empty
    | (str, styp)::t -> (match styp with
			   | Int -> Smap.add str (IntT(Imap.empty)) (init t)
			   | String -> Smap.add str (StringT(Smap.empty)) (init t));;										


(*********************************************************************)								   
(*                fonction d'ajout pour un ldap                      *)
(*********************************************************************)

(* messages d'erreur pour la fonction add_aux *)
(* cas où le string du record n'existe pas dans le ldap *)
let error_output1 = "Incorrect record, unable to find matching field in existing ldap";; 
(* cas où le type de la donnée ajoutée diffère de celui de la donnée déjà existante *)
let error_output2 = "Incorrect record, conflicting type with existing field";;

(* add_aux reco ld id retourne le ldap ld auquel on a ajouté le record reco d'identifiant unique id *)
(* fonction auxiliaire de add à laquelle on passe en plus l'identifiant unique créé en argument *)			
let rec add_aux reco ld id=
  match reco with
    | [] -> ld
    | (str, vtyp)::t -> (* on regarde si la table que l'on veut ajouter est déjà définie, et on la récupère *)
	let pmap = (try Smap.find str ld with | Not_found -> raise (SchemaError error_output1) ) in (* sinon, retourne un message d'erreur *)
	  (match vtyp with
	     | IntV(n) -> (* dans le cas d'une table de type entier *)
		 (match pmap with
		    | StringT(smap) -> raise (SchemaError error_output2)
		    | IntT(imap) -> (* on regarde si il existe déjà un champ entier n, et on le récupère *)
			let uset = try Imap.find n imap with |Not_found -> Pset.empty in (* sinon, on le crée *)
			  add_aux t (Smap.add str (IntT(Imap.add n (Pset.add id uset) imap)) ld) id) (* ajout des valeurs au champ*)
	     | StringV(s) -> (match pmap with (* dans le cas d'une table de type string *)
				| IntT(imap) -> raise (SchemaError error_output2)
				| StringT(smap) -> (* on regarde si il existe déjà un champ string s, et on le récupère *)
				    let uset = try Smap.find s smap with |Not_found -> Pset.empty in (* sinon, on le crée *)
				      add_aux t (Smap.add str (StringT(Smap.add s (Pset.add id uset) smap)) ld) id));;(* ajout des valeurs au champ*)


(* add reco ld retourne le ldap ld auquel on a ajouté le record reco *)
let add reco ld = add_aux reco ld (create reco);; (*création de l'identifiant unique, qui est passé à add_aux *)


(*********************************************************************)								   
(*                fonction de retrait pour un ldap                   *)
(*********************************************************************)

(* messages d'erreur supplémentaires pour la fonction remove *)
(* cas où la valeur que l'on veut supriemr n'existe pas dans le champs *)
let error_output3 = "Incorrect record, unable to find matching value in ldap";;
(* cas où l'identifiant que l'on souhaite n'existe pas dans le pset *)
let error_output4 = "Incorrect record, unable to find matching unique identifiant in ldap";;


(* check_empty_uset_int value id uset pmap str ld vérifie si le uset, après suppression de l'indentifiant id, est vide ou non. Si oui, il est supprimé *)
(* pour un Imap *)
let check_empty_uset_int value id uset pmap str ld =
  let new_uset = try Pset.remove id uset with (* on supprime l'identifiant de uset *)
    | Not_found -> raise (SchemaError error_output4) in (* si celui-ci ne contient pas l'identifiant, on renvoit une erreur *)
    if new_uset = Pset.empty then (* si le nouveau uset auquel on a supprimé l'identifiant est vide *)
      Smap.add str (IntT(Imap.remove value pmap)) ld (* on enlève la valeur à laquelle était associée l'identifiant *)
    else
      Smap.add str (IntT(Imap.add value new_uset pmap)) ld;; (* sinon, on met à jour le ldap ld avec le nouveau uset *)


(* check_empty_uset_string value id uset pmap str ld vérifie si le uset, après suppression de l'indentifiant id, est vide ou non. Si oui, il est supprimé *)
(* pour un Smap *)			
let check_empty_uset_string value id uset pmap str ld =
  let new_uset = try Pset.remove id uset with (* on supprime l'identifiant de uset *)
    | Not_found -> raise (SchemaError error_output4) in (* si celui-ci ne contient pas l'identifiant, on renvoit une erreur *)
    if new_uset = Pset.empty then (* si le nouveau uset auquel on a supprimé l'identifiant est vide *)
      Smap.add str (StringT(Smap.remove value pmap)) ld (* on enlève la valeur à laquelle était associée l'identifiant *)
    else
      Smap.add str (StringT(Smap.add value new_uset pmap)) ld;;	(* sinon, on met à jour le ldap ld avec le nouveau uset *)		


(* remove reco ld retourne le ldap ld auquel on a retiré le record reco *)
(* les messages d'erreurs 1 et 2 sont les mêmes que pour l'ajout *)			
let rec remove_aux reco ld id =
  match reco with
    | [] -> ld
    | (str, vtyp)::t -> (* on regarde si la table que l'on veut retirer existe bien, et on la récupère *)
	let pmap = (try Smap.find str ld with | Not_found -> raise (SchemaError error_output1) ) in (* sinon, retourne un message d'erreur *)
	  (match vtyp with
	     | IntV(n) -> (* dans le cas d'une table de type entier *)
		 (match pmap with
		    | StringT(smap) -> raise (SchemaError error_output2)
		    | IntT(imap) -> (* on regarde si il existe bien un champ entier n, et on le récupère *)
			let uset = try Imap.find n imap with 
			  |Not_found -> raise (SchemaError error_output3) in (* sinon, on retourne une erreur *)
			  remove_aux t (check_empty_uset_int n id uset imap str ld) id) (* appelle de la fonction de suppression de l'id *)
		   
	     | StringV(s) -> (match pmap with (* dans le cas d'une table de type string *)
				| IntT(imap) -> raise (SchemaError error_output2)
				| StringT(smap) -> (* on regarde si il existe déjà un champ string s, et on le récupère *)
				    let uset = try Smap.find s smap with 
				      |Not_found -> raise (SchemaError error_output3) in (* sinon, on retourne une erreur *)
				      remove_aux t (check_empty_uset_string s id uset smap str ld) id));; (* appelle de la fonction de suppression de l'id *)


let remove reco ld = remove_aux reco ld (create reco);; (*création de l'identifiant unique du record reco, qui est passé à remove_aux *)


(*********************************************************************)								   
(*                fonction de requête pour un ldap                   *)
(*********************************************************************)

(* query ldapdb q retourne, sur la donnée d'une base ldap ldapdb, un ensemble d'enregistrement répondant à la requête q *)
let rec query ldapdb q = 
  match q with
    | And (q1, q2) -> Pset.inter (query ldapdb q1) (query ldapdb q2)
    | Or (q1, q2) -> Pset.union (query ldapdb q1) (query ldapdb q2)
    | Filter (name, func) -> if ldapdb = Smap.empty then raise (SchemaError "Empty ldap") 
      else let pmap = try Smap.find name ldapdb with | Not_found -> raise (SchemaError "Incorrect query") in
    	func pmap;;


(*********************************************************************)								   
(*                fonctions d'affichage pour un ldap                  *)
(*********************************************************************)

(* print_record r imprime la description (noms de champ et valeurs) de l'enregistrement r, sous le format : une propriété par ligne, un saut de ligne à la fin *)
let rec print_record r =
  match r with
    | [] -> ()
    | (str, vtyp)::t -> (match vtyp with
			   | IntV(n) -> let _ = print_endline ("Champ = "^str^", Valeur = "^(string_of_int n)) in
			       print_record t
			   | StringV(s) -> let _ = print_endline ("Champ = "^str^", Valeur = "^s) in
			       print_record t);;


(* print_id id imprime l'enregistrement correspondant à l'identifiant id, sous le même format que print_record *)
let print_id id =
  print_record (Unique.value id);;
