open Bap_report.Std

type t

val create :
  ?confirmations:string ->
  ?store:(string * bool) ->
  output:string ->
  Job.ctxt ->
  t

val run : t -> (artifact list * recipe list) list -> int -> unit

val of_incidents_file : t -> string -> unit

val of_db : t -> string -> unit
