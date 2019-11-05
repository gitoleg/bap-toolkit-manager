open Core_kernel
open Bap_report_types

type recipe = Bap_report_recipe.t
type limit  = Bap_report_limit.t
type journal = Bap_report_journal.t

val create :
  ?limit:limit ->
  ?verbose:bool ->
  ?watch:bool ->
  pwd:string ->
  workdir:string ->
  path:string ->
  recipe -> string * journal
