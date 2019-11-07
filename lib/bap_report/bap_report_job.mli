open Core_kernel
open Bap_report_types

type recipe = Bap_report_recipe.t
type limit = Bap_report_limit.t
type tool = Bap_report_tool.t
type journal = Bap_report_journal.t

type steady
type ready

type 'a t
type ctxt

type save = [
  | `Nothing
  | `Service
  | `Everything
]

val context : ?save:save -> ?limit:limit -> string -> tool -> ctxt

val prepare : ctxt -> recipe -> file -> steady t

val run : ctxt -> steady t -> ready t Or_error.t

val journal : 'a t -> journal

val name : 'a t -> string
