open Core_kernel
open Cmdliner
open Bap_report.Std

module Cmd = Bap_report_cmd_terms

type mode =
  | From_schedule of string
  | From_incidents of string
  | From_stored of string
  | Run_artifacts


type ctxt = {
  tool    : image;
  limit   : limit;
  verbose : bool;
}

type t = {
  mode       : mode;
  context    : ctxt;
  artifacts  : string list;
  recipes    : recipe list;
  confirms   : string option;
  output     : string;
  view       : string option;
  store      : string option;
  update     : bool;

} [@@deriving fields]

let make_recipe tool r =
  let name = Cmd.requested_name r in
  match Recipe.find tool name with
  | None -> Or_error.errorf "can't find recipe %s\n" name
  | Some recipe ->
    let recipe =
      List.fold (Cmd.requested_pars r) ~init:recipe
        ~f:(fun recipe (p,v) ->
            Recipe.add_parameter recipe p v) in
    Ok recipe

let create_recipes tool recipes =
  Result.all @@ List.map recipes ~f:(make_recipe tool)

let create mode ctxt artis recipes a b c d e =
  match create_recipes ctxt.tool (List.concat recipes) with
  | Error er ->
    eprintf "%s\n" @@ Error.to_string_hum er;
    exit 1
  | Ok recipes ->
     Fields.create mode ctxt artis recipes a b c d e

let infer_mode schedule of_file of_incidents =
  match schedule, of_file, of_incidents with
  | Some f,_,_ -> From_schedule f
  | _,Some f,_ -> From_stored f
  | _,_,Some f -> From_incidents f
  | _ -> Run_artifacts

let context tool limits verbose =
  match Docker.Image.of_string tool with
  | Error er ->
     eprintf "can't find tool %s: %s" tool (Error.to_string_hum er);
     exit 1
  | Ok tool ->
  let limit = List.fold limits
                ~init:Limit.empty ~f:(fun l (n,q) -> Limit.add l n q) in
  {tool; verbose; limit}

open Cmd

let options =
  let mode = Term.(const infer_mode
                   $schedule $of_file $of_incidents) in
  let ctxt = Term.(const context $tool $limits $verbose) in
  Term.(const create
        $mode
        $ctxt
        $artifacts
        $recipes
        $confirms
        $output
        $view
        $store
        $update)
