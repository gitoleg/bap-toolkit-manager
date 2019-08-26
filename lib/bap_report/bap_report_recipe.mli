open Core_kernel

type t

val find : string -> t option

val list : ?tool:Bap_report_tool.t -> unit -> t list

val name : t -> string

val description : t -> string

val run : ?tool:Bap_report_tool.t -> ?image:string -> ?tag:string -> string -> t -> t

val time_taken : t -> float
