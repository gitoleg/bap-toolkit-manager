open Core_kernel
open Bap_report_utils


module Filename = Caml.Filename
module Docker = Bap_report_docker

type image = Bap_report_docker.image

let split_string s =
  String.split_on_chars ~on:[' '; '\t'] s |>
  List.filter ~f:(fun s -> s <> "") |>
  List.map ~f:String.strip


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
    args : string option;
  }


let drive = "/mydrive"
let pwd = Sys.getcwd

let name t = t.name
let description t = t.desc

let list tool =
  let recipe_of_string s =
    match split_on_first ~on:[' '; '\t'] s with
    | name :: desc :: _ ->
       let name = String.strip name in
       let desc = String.strip desc in
       Some {name;desc;args=None}
    | _ -> None in
  match Docker.run tool "--list-recipes" with
  | None | Some "" -> []
  | Some r ->
     let rs = String.split ~on:'\n' r in
     List.filter_map rs ~f:recipe_of_string

let find tool name' =
  List.find (list tool) ~f:(fun {name} -> String.equal name name')

let with_args t args = {t with args=Some args}

let script dir target recipe =
  let recipe = match recipe.args with
    | None -> recipe.name
    | Some args -> sprintf "%s:%s" recipe.name args in
  [ "#!/usr/bin/env sh\n";
    sprintf "cd %s" drive;
    sprintf "mkdir %s" dir;
    sprintf "cd %s" dir;
    sprintf "bap %s/%s --recipe=%s -d -dasm > bap.stdout" drive target recipe;
    "cd ../";
    sprintf "tar czf %s.tgz %s" dir dir;
    sprintf "rm -r %s" dir;
  ] |> String.concat ~sep:"\n"

let entry workdir target recipe =
  let s = script workdir target recipe in
  let tmp = Filename.temp_file ~temp_dir:(pwd ()) "script" "" in
  Out_channel.with_file tmp ~f:(fun c -> Out_channel.output_lines c [s]);
  Unix.chmod tmp 0o777;
  tmp

let copy_target ?image ~path alias =
  ignore @@
    match image with
    | None -> cmd "cp %s %s/%s" path (pwd ()) alias
    | Some image ->
       Docker.run ~mount:(pwd (), drive) image
       @@ sprintf "cp %s %s/%s" path drive alias

let workdir ?image path recipe =
  match image with
  | None -> sprintf "%s.%s" (Filename.basename path) recipe
  | Some im ->
     match Docker.Image.tag im with
     | None ->
        sprintf "%s.%s" (Docker.Image.to_string im) recipe
     | Some tag -> sprintf "%s.%s" tag recipe

let run t ~tool ?image path =
  let alias = Filename.temp_file ~temp_dir:(pwd ()) "artifact" "" in
  copy_target ?image ~path (Filename.basename alias);
  let workdir = workdir ?image path t.name in
  let entry = entry workdir (Filename.basename alias) t in
  let start = Unix.gettimeofday () in
  let _ = Docker.run tool ~mount:(pwd (), drive)
            ~entry:(sprintf "%s/%s" drive @@ Filename.basename entry) "" in
  let finish = Unix.gettimeofday () in
  Sys.remove alias;
  Sys.remove entry;
  finish -. start
