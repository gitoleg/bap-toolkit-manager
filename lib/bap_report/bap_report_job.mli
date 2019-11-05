open Core_kernel
open Bap_report_types

type recipe = Bap_report_recipe.t
type limit = Bap_report_limit.t
type tool = Bap_report_tool.t

type steady
type ready

type 'a t
type ctxt

val context : ?verbose:bool -> ?limit:limit -> tool -> ctxt

val prepare : ctxt -> recipe -> file -> steady t

val run : ctxt -> steady t -> ready t Or_error.t

val journal : 'a t -> Bap_report_script.journal

val name : 'a t -> string
