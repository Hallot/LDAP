
type 'a t
type key = char
 
(** The empty map *)
val empty : 'a t
 
(** mem x m returns true if m contains a binding for x, and false otherwise. *)
val mem : key -> 'a t -> bool
 
(** find x m returns the element associated to x if m contains a binding for x, 
    raise Not_found otherwise *)
val find : key -> 'a t -> 'a
 
(** add x y m returns a map containing the same bindings as m, plus a binding of
    x to y. If x was already bound in m, its previous binding disappears. *)
val add : key -> 'a -> 'a t -> 'a t
 
(** remove x m returns a map containing the same bindings as m, except for x
    which is unbound in the returned map. *)
val remove : key -> 'a t -> 'a t

