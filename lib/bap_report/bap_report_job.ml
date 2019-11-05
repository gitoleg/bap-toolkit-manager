open Core_kernel
open Bap_report_utils
open Bap_report_types
open Bap_report_read

module Recipe  = Bap_report_recipe
module Script  = Bap_report_script
module Journal = Script.Journal
module Limit   = Bap_report_limit
module Tool    = Bap_report_tool

type tool = Bap_report_tool.t
type recipe = Bap_report_recipe.t
type limit = Bap_report_limit.t

type journal = Journal.t

type steady = {
  arti  : string;
  entry : string;
}

type ready = unit

type 'a t = {
  name : string;
  journal : journal;
  payload : 'a;
}

type ctxt = {
  tool    : tool;
  limit   : limit;
  verbose : bool;
}

let drive = "/mydrive"
let pwd = Sys.getcwd

let journal t = t.journal

let entry script =
  let tmp = Filename.temp_file ~temp_dir:(pwd ()) "script" "" in
  Out_channel.with_file tmp ~f:(fun c -> Out_channel.output_lines c [script]);
  Unix.chmod tmp 0o777;
  tmp

let copy_target file alias =
  ignore @@
  match File.image file with
  | None -> cmd "cp %s %s/%s" (File.path file) (pwd ()) alias
  | Some image ->
    Image.run ~mount:(pwd (), drive) image
    @@ sprintf "cp %s %s/%s" (File.path file) drive alias

let workdir file recipe =
  match File.image file with
  | None -> sprintf "%s.%s" (Filename.basename @@ File.path file) recipe
  | Some im ->
    match Image.tag im with
    | None -> sprintf "%s.%s" (Image.to_string im) recipe
    | Some tag -> sprintf "%s.%s" tag recipe

let context ?(verbose=true) ?(limit=Limit.empty) tool = {verbose; limit; tool}

let apply tool entry =
  match Tool.image tool with
  | Some im ->
    Image.run im ~mount:(pwd (), drive)
      ~entry:(sprintf "%s/%s" drive @@ Filename.basename entry) ""
  | None -> cmd "sh %s" entry

let remove x =
  try Sys.remove x
  with _ -> ()

let prepare {verbose; tool; limit} recipe file =
  let name = sprintf "%s:%s" (File.path file) (Recipe.name recipe) in
  let alias = Filename.temp_file ~temp_dir:(pwd ()) "artifact" "" in
  copy_target file (Filename.basename alias);
  let workdir = workdir file (Recipe.name recipe) in
  let script,journal =
    let pwd =
      if Option.is_none (Tool.image tool) then pwd ()
      else drive in
    Script.create ~limit ~pwd ~verbose ~workdir
      ~path:(Filename.basename alias) recipe in
  let entry = entry script in
  {journal;payload={entry; arti=alias}; name}

let run {tool;} ({payload={entry;arti}; } as t) =
  let res = apply tool entry in
  remove arti;
  remove entry;
  match res with
  | Ok _ -> Ok { t with payload = (); }
  | Error _ as er -> er

let name t = t.name
