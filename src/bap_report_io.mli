open Core_kernel
open Bap_report.Std

module Artifacts : sig
  val dump : string -> artifact list -> unit
  val read : string -> artifact list Or_error.t
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

  val read : In_channel.t -> t option
  val write : Out_channel.t -> t -> unit
end
