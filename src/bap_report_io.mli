open Bap_report.Std

module Artifacts : sig
  val dump : ?update:bool -> string -> artifact list -> unit
  val read : string -> artifact list
end

module Msg : sig
  type job_id = string [@@deriving bin_io, sexp]

  type t = [
    | `Job_started of job_id
    | `Job_finished of job_id
    | `Job_errored of job_id
    | `Job_incidents of job_id * int
    | `Tick
  ] [@@deriving bin_io, sexp]

  val read : in_channel -> t option
  val write : out_channel -> t -> unit
end
