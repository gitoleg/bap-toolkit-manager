open Bap_report_common

type t [@@deriving sexp,compare,bin_io]

type kind [@@deriving sexp,compare,bin_io]

val create : kind ->
             Bap_report_incident.kind ->
             addr list -> t

val locations : t -> addr list
val kind : t -> Bap_report_incident.kind
val confirmation : t -> kind

val validate : t -> status option -> status
