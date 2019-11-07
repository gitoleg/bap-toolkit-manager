open Core_kernel
open Bap_report_utils
open Bap_report_types
open Bap_report_read
open Bap_report_env

module Recipe  = Bap_report_recipe
module Journal = Bap_report_journal
module Limit   = Bap_report_limit
module Tool    = Bap_report_tool

type tool = Bap_report_tool.t
type recipe = Bap_report_recipe.t
type limit = Bap_report_limit.t

type journal = Journal.t

type save = [
  | `Nothing
  | `Service
  | `Everything
]

module Script = struct

  type run = {
      path    : string;
      recipe  : recipe;
      watch   : bool;
      save    : save;
    }

  type insn =
    | Chdir of string
    | Mkdir of string
    | Rmdir of string
    | Limit of limit
    | Write of journal
    | RunIt of run


  let string_of_run {path; recipe; watch; save} =
    String.concat ~sep:" " [
        if watch then sprintf "/usr/bin/time -v -o %s" mytime else "";
        "bap";
        path;
        sprintf "--recipe=%s" (Recipe.to_string recipe);

        (match save with
         | `Everything ->
            "-dbir:out.bir --print-bir-attr=address -dasm:out.asm -dogre:out.ogre -dknowledge:out.knowledge"
         | _ -> "");

        sprintf "> %s 2> %s" stdout stderr;
      ]

  let render insns =
    let rec loop acc = function
      | [] -> List.rev acc
      | x :: xs ->
         let insns = match x with
           | Chdir dir -> [sprintf "cd %s" dir]
           | Mkdir dir -> [sprintf "mkdir %s" dir]
           | Rmdir dir -> [sprintf "rm -r %s" dir]
           | Limit lmt -> [Limit.string_of_t lmt]
           | Write jrn -> [Journal.write_cmd jrn]
           | RunIt run -> [string_of_run run] in
         loop (insns @ acc) xs in
    let header = "#!/usr/bin/env sh\n"; in
    loop [header] insns |> String.concat ~sep:"\n"

  let create
        ?(limit=Limit.empty)
        ?(watch=true)
        ~save
        ~pwd
        ~workdir ~path recipe journal =
    render [
        Limit limit;
        Chdir pwd;
        Mkdir workdir;
        Chdir workdir;
        RunIt {save;watch;recipe;path=sprintf "%s/%s" pwd path};
        Chdir pwd;
        Write journal;
        Rmdir workdir
      ]
end



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
  tool       : tool;
  limit      : limit;
  save       : save;
  root       : string;
}

let drive = "/mydrive"

let journal t = t.journal

let entry jobsdir script =
  let tmp = Filename.temp_file ~temp_dir:jobsdir "script" "" in
  Out_channel.with_file tmp ~f:(fun c -> Out_channel.output_lines c [script]);
  Unix.chmod tmp 0o777;
  tmp

let copy_target jobsdir file alias =
  ignore @@
  match File.image file with
  | None -> cmd "cp %s %s/%s" (File.path file) jobsdir alias
  | Some image ->
    Image.run ~mount:(jobsdir, drive) image
    @@ sprintf "cp %s %s/%s" (File.path file) drive alias

let workdir file recipe =
  match File.image file with
  | None -> sprintf "%s.%s" (Filename.basename @@ File.path file) recipe
  | Some im ->
    match Image.tag im with
    | None -> sprintf "%s.%s" (Image.to_string im) recipe
    | Some tag -> sprintf "%s.%s" tag recipe

let context ?(save=`Nothing) ?(limit=Limit.empty) root tool =
  if not (Sys.file_exists root) then
    Unix.mkdir root 0o777;
  {save; limit; tool; root}

let apply root tool entry =
  match Tool.image tool with
  | Some im ->
    Image.run im ~mount:(root, drive)
      ~entry:(sprintf "%s/%s" drive @@ Filename.basename entry) ""
  | None -> cmd "sh %s" entry

let remove x =
  try Sys.remove x
  with _ -> ()

let prepare {save; tool; limit;root} recipe file =
  let name = sprintf "%s:%s" (File.path file) (Recipe.name recipe) in
  let alias = Filename.temp_file ~temp_dir:root "artifact" "" in
  copy_target root file (Filename.basename alias);
  let workdir = workdir file (Recipe.name recipe) in
  let journal = Journal.of_workdir @@ sprintf "%s/%s" root workdir in
  let script =
    let pwd =
      if Option.is_none (Tool.image tool) then root
      else drive in
    Script.create ~limit ~pwd ~save ~workdir
      ~path:(Filename.basename alias) recipe journal in
  let entry = entry root script in
  {journal;payload={entry; arti=alias}; name}

let run {tool;root} ({payload={entry;arti}; } as t) =
  let res = apply root tool entry in
  remove arti;
  remove entry;
  match res with
  | Ok _ -> Ok { t with payload = (); }
  | Error _ as er -> er

let name t = t.name
