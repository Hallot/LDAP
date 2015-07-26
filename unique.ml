(**********************************************************************)
(**********************************************************************)
(**********************************************************************)

module type UniqueType = sig
  (** type of unique labels *)
  type t
    (** type of values *)
  type v
    
  (** create v returns a unique label t for a value v *)
  val create : v -> t
    
  (** value i returns the value corresponding to label i *)
  val value : t -> v

(*  val uid : t -> int *)
end

(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
  
module type OrderedType = sig type t val compare : t -> t -> int end
  
(* This module gives to each value a single id *)
module UniqueMake ( S : OrderedType ) : UniqueType with type v = S.t = struct
  
  module VMap = Map.Make(S)
    
  type v = S.t
  type t = Hash of (v * int)
    
  let gentag =
    let r = ref 0 in
      fun () -> incr r; !r
	
  let cons = ref VMap.empty
    
  let create v =
    try Hash(v,VMap.find v !cons) with
	Not_found -> begin
	  let i = gentag () in
	    cons := VMap.add v i !cons;
	    Hash (v,i)
	end
	  
  let value = function Hash (v,_) -> v
  let uid = function Hash (_,i) -> i
    
end
