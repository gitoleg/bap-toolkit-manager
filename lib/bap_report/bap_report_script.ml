open Core_kernel
open Bap_report_types
open Bap_report_utils
open Bap_report_read
open Bap_report_env

module Recipe = Bap_report_recipe
module Limit  = Bap_report_limit
module Journal = Bap_report_journal

type recipe = Recipe.t
type limit  = Limit.t
type journal = Journal.t

type run = {
  path    : string;
  recipe  : recipe;
  watch   : bool;
  verbose : bool;
}

type insn =
  | Chdir of string
  | Mkdir of string
  | Rmdir of string
  | Limit of limit
  | Write of journal
  | RunIt of run


let string_of_run {path; recipe; watch; verbose} =
  String.concat ~sep:" " [
    if watch then sprintf "/usr/bin/time -v -o %s" mytime else "";
    "bap";
    path;
    sprintf "--recipe=%s" (Recipe.to_string recipe);
    if verbose
    then
      "-dbir:out.bir --print-bir-attr=address -dasm:out.asm -dogre:out.ogre -dknowledge:out.knowledge" else "";
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
    ?(verbose=true)
    ?(watch=true)
    ~pwd
    ~workdir ~path recipe =
  let journal = Journal.of_workdir workdir in
  render [
    Limit limit;
    Chdir pwd;
    Mkdir workdir;
    Chdir workdir;
    RunIt {verbose;watch;recipe;path=sprintf "%s/%s" pwd path};
    Chdir pwd;
    Write journal;
    Rmdir workdir
  ], journal
