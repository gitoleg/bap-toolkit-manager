open Core_kernel

type t

val of_workdir : string -> t

(* returns a command relatively to the (basename workdir*)
val write_cmd : t -> string

val incidents : t -> Bap_report_types.incident list
val errors : t -> string list
val time : t -> float option
val equal : t -> t -> bool

val incidents' : ?bookmark:int64 -> t -> int * int64

val remove : t -> unit

include Identifiable.S with type t := t
