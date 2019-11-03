open Bap_report.Std

module Artifacts : sig
  val dump : ?update:bool -> string -> artifact list -> unit
  val read : string -> artifact list
end

module Msg : sig
  type t =
    | Job_started of string
    | Job_finished of string
    [@@deriving bin_io]

  val read : in_channel -> t option
  val write : out_channel -> t -> unit
end
