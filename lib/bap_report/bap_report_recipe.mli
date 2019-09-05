open Core_kernel

type t
type image = Bap_report_docker.image

val find : image -> string -> t option
val list : image -> t list

val with_args : t -> string -> t

val name : t -> string

val description : t -> string

val run : t -> tool:image -> ?image:image -> string -> float
