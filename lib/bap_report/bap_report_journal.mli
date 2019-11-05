open Core_kernel

type t

val of_workdir : string -> t
val write_cmd : t -> string
val incidents : t -> Bap_report_types.incident list
val errors : t -> string list
val time : t -> float option
val equal : t -> t -> bool

val stat : ?pos:int64 -> t -> int



include Identifiable.S with type t := t
