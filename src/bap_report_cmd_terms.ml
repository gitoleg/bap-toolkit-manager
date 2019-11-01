open Core_kernel
open Cmdliner
open Bap_report.Std

type requested_recipe = {
  name : string;
  pars : (string * string) list
}

let requested_name {name} = name
let requested_pars {pars} = pars

module Recipes = struct
  type t = requested_recipe list

  let printer fmt recipes =
    List.iter recipes ~f:(fun {name;pars} ->
        let args = List.map pars ~f:(fun (a,b) -> sprintf "%s=%s" a b) in
        Format.fprintf fmt "%s with %s\n" name
          (String.concat ~sep:"," args))

  let with_no_pars xs = List.map xs ~f:(fun x -> {name=x; pars=[]})

  let parse_par a =
    match String.split a ~on:'=' with
    | [arg;value] -> arg,value
    | _ ->
      eprintf "can't parse argument %s, should be in the form arg=value\n" a;
      exit 1

  let parser : t Arg.parser = fun str ->
    match String.split ~on:':' str with
    | [names]  -> `Ok (with_no_pars (String.split ~on:',' names))
    | [name;pars] ->
      let pars = String.split pars ~on:',' in
      let pars = List.map pars ~f:parse_par in
      `Ok ([{name; pars}])
    | _ -> `Error (sprintf
                     "don't know what to do with %s, see help for details" str)

  let conv : t Arg.conv = parser,printer

end

module Limit_arg = struct
  open Limit

  type t = int * quantity

  let printer fmt (n, q) =
    Format.fprintf fmt "%d %s" n (string_of_quantity q)

  let chop_suffix str suf = match suf with
    | None -> Some str
    | Some suffix -> String.chop_suffix str ~suffix

  let nums = Str.regexp "[0-9]+"

  let parser s =
    let error =
      `Error (sprintf  "string '%s' doesn't fit to limit format" s) in
    if Str.string_match nums s 0 then
      if Str.match_beginning () = 0 then
        let num = Str.matched_string s in
        match String.chop_prefix ~prefix:num s with
        | None | Some "" -> error
        | Some suf ->
          match quantity_of_string suf with
          | Some q -> `Ok (int_of_string num,q)
          | None -> error
      else error
    else error

  let conv : t Arg.conv = parser,printer
end

let doc = "Bap toolkit"

let man = [
  `S "SYNOPSIS";
  `Pre "
      $(mname) --artifacts=... --recipes=...
      $(mname) --artifacts=... --recipes=... --confirmations=...
      $(mname) --schedule=...
      $(mname) --list-recipes
      $(mname) --list-artifacts";

  `S "Description";

  `P "A frontend to the whole bap and docker infrastructure
      that hides all the complexity under the hood: no bap
      installation required, no manual pulling of docker
      images needed, no complex command line options needed.";

  `P "It allows easily to run the various of checks against the various
      of artifacts and get a frendly HTML report with all the
      incidents found. An artifact is out parlance is just a file
      that is an object of analysis.";

  `P "The whole idea of framework is based on the next formula:
      $(b, Artifact + Recipe = Report).
      And everything else is just a fairly small set of useful options
      to make a result better to look and/or easier to get.";

  `P  "Also, $(mname) records the information provided both by
       BAP and a particular analysis: BIR program, assembly,
       logs, error messages and etc. A storage with this data is
       called a $(i,journal). A journal is created for each
       combination of artifact and analysis and phisically represented
       as an archive on a hard drive.";

]

let info = Term.info ~version:"dev" ~man ~doc "bap-tookit"

let schedule =
  let doc = "creates a schedule of artifacts and recipes to run
             from a provided file, that contains s-expressions in
             the form:
             (artifact1 (recipe1 recipe2))
             (artifact2 recipe1)
             (artifact3 all)
             ... " in
  Arg.(value & opt (some string) None & info ~doc ["schedule"; "s"])

let strings = Arg.(list string)

let artifacts =
  let doc = "A comma-separated list of artifacts to check.
             Every artifact is either a file in the system
             or a TAG from $(i,binaryanalysisplatform/bap-artifacts)
             docker image" in
  Arg.(value & opt_all strings [] & info ["artifacts"; "a"] ~doc)

let recipes : Recipes.t list Term.t =
  let doc = "a comma-separated list of the recipes to run.
             A special key $(i,all) can be used to run all the recipes.
             A recipe with parameters should be set individually with
             the same arg:
             --recipes=r1 --recipes=r2:par1=val1,par2=val2
             OR
             -r r1 -r r2:par1=val1,par2=val2 -r r3" in
  Arg.(value & opt_all Recipes.conv [] & info ["recipes"; "r"] ~doc)

let confirms =
  let doc = "file with confirmations.
  A confirmation is a file with the of expectations from
  an application of an analysis to an artifact. It can alter
  an appearance of incidents in a report." in
  Arg.(value & opt (some non_dir_file) None & info ["confirmations"; "c"] ~doc)

let report =
  let doc = "an output file with html report" in
  Arg.(value & opt string "results.html" & info ["report";] ~doc)

let list_recipes =
  let doc = "prints the list of available recipes and exits" in
  Arg.(value & flag & info ["list-recipes"] ~doc)

let list_artifacts =
  let doc = "prints list of available artifacts and exits" in
  Arg.(value & flag & info ["list-artifacts"] ~doc)

let bap_version =
  let doc = "print bap version that is used in a tool and exits" in
  Arg.(value & flag & info ["bap-version"] ~doc)

let of_incidents =
  let doc = "create a report from file with incidents" in
  Arg.(value & opt (some non_dir_file) None & info ["of-incidents"; "i"] ~doc)

let tool =
  let default = "binaryanalysisplatform/bap-toolkit:latest" in
  let doc = "A tool used to run analysis.
             Tags could be fed as expected, with the ':' separator.
             A special keyword $(b,host) can be used to use host
             bap and recipes" in
  let tool_info = Arg.info ["tool"; "t"] ~doc in
  Arg.(value & opt string default tool_info)

let config =
  let doc = "Config file contains information that is useful
             for display purposes." in
  Arg.(value & opt (some non_dir_file) None & info ["config";] ~doc)

let store =
  let doc = "store results in the file" in
  Arg.(value & opt (some string) None & info ["store"] ~doc)

let update =
  let doc = "update file with results (e.g. run another analysis)" in
  Arg.(value & flag & info ["update"] ~doc)

let of_file =
  let doc = "create a report from previously stored data" in
  Arg.(value & opt (some string) None & info ["from";] ~doc)

let limits =
  let doc =
    "Set a memory/time limit per running recipe.
     Job will be canceled if a limit exceeded be canceled.
     Possible limitations:
     for time:
        10s - 10 seconds
        10m - 10 minutes
        10h - 10 hours;
     for resident memory:
        10Mb - 10 Megabytes
        10Gb - 10 Gigabytes" in
  Arg.(value & opt_all Limit_arg.conv [] & info ["limit"; ] ~doc)

let verbose =
  let doc = "Establish a high level of verbosity for a journal,
  so it will save as much of the information as possible.
  If set to $(i,false), then no BIR of assembly output will be
  stored, but it can make an analysis run faster" in
  Arg.(value & opt bool true & info ["verbose";] ~doc)

let jobs =
  let doc = "Run few analysis simultaneously" in
  Arg.(value & opt int 1 & info ["jobs"; "j"] ~doc)

let disable_journal =
  let doc = "Don't preserve the results of an analysis" in
  Arg.(flag & info ["disable-journal"] ~doc)

let workdir =
  let doc = "Stores all the journals directory" in
  Arg.(value & opt string "/tmp" & info ["workdir";] ~doc)
