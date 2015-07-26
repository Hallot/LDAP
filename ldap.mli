
type vtypes = IntV of int | StringV of string
type record = (string * vtypes) list

(* Ensemble d'éléments uniques *)
module Unique :  Unique.UniqueType with type v = record

type uset = Unique.t Pset.t


type itypes = IntT of uset Imap.t | StringT of uset Smap.t

type stypes = Int | String
type schema = (string * stypes) list
type ldap = itypes Smap.t

type querytype =
  |And of querytype * querytype
  |Or of querytype * querytype
  |Filter of (string * (itypes -> uset))

exception SchemaError of string

val init : schema -> ldap
val add : record -> ldap -> ldap
val remove : record -> ldap -> ldap
val query : ldap -> querytype -> uset
val print_record : record -> unit
val print_id : Unique.t -> unit
