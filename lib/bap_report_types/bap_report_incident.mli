open Core_kernel
open Bap_report_common

type t [@@deriving bin_io, compare, sexp]

type kind  [@@deriving bin_io, compare, sexp]

val kind_of_string : string -> kind
val string_of_kind : kind -> string


val create : ?path:string list ->
             ?locs:addr list -> kind -> addr -> t

val addr : t -> addr
val locations : t -> addr list
val path : t -> string list
val kind : t -> kind


module Map : Map.S with type Key.t = t
module Set : Set.S with type Elt.t = t
