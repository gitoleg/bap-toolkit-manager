open Core_kernel
open Bap_report_utils
open Bap_report_types

module Filename = Caml.Filename


let split_on_first s ~on =
  let indexes = List.filter_map on ~f:(String.index s) in
  let indexes = List.sort ~compare:Int.compare indexes in
  match indexes with
  | [] -> [s]
  | i :: _ ->
    [String.subo ~len:i s;
     String.subo ~pos:(i + 1) s]

type t =  {
  name : string;
  desc : string;
  args : (string * string) list;
  kinds : incident_kind list;
}

type recipe = t

let name t = t.name
let description t = t.desc
let provide t k = {t with kinds = k :: t.kinds}
let kinds t = t.kinds

let create ~name ~desc =
  {name;desc;args=[];kinds=[]}

let add_parameter t ~name ~value =
  { t with args = (name,value) :: t.args }

let to_string recipe = match recipe.args with
  | [] -> recipe.name
  | args ->
    let args = List.map args ~f:(fun (a,v) -> sprintf "%s=%s" a v) in
    let args = String.concat ~sep:"," args in
    sprintf "%s:%s" recipe.name args
