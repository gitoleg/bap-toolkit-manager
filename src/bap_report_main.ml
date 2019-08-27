open Core_kernel
open Bap_report.Std
open Bap_report_options

module Bap_artifact = struct
  let image = "binaryanalysisplatform/bap-artifacts"

  type kind =
    | Local
    | Image

  let run_recipe tool a kind r =
    match kind with
    | Local -> Recipe.run ~tool (Artifact.name a) r
    | Image ->  Recipe.run ~tool ~image ~tag:(Artifact.name a) "/artifact" r

  let can't_find tag reason =
    eprintf "can't find %s: %s\n" tag reason

  let artifact_exists tag =
    match Docker.get_image ~tag image with
    | Error er ->
      can't_find tag (Error.to_string_hum er);
      false
    | Ok () ->
      let cmd = sprintf "find / -type f -name /artifact" in
      match Docker.run ~image ~tag cmd with
      | None | Some "" ->
        can't_find tag "no such file in image";
        false
      | _ -> true

  let kind_of_name name =
    if Sys.file_exists name then Some Local
    else if artifact_exists name then Some Image
    else None

  let find name =
    match kind_of_name name with
    | None -> None
    | Some Local ->
      let size = Size.get name in
      Some (Local, Artifact.create ?size name)
    | Some Image ->
      let size = Size.get ~image ~tag:name "/artifact" in
      Some (Image, Artifact.create ?size name)

end

let check_equal x y = compare_incident_kind x y = 0

let check_diff xs ys =
  List.fold xs ~init:[] ~f:(fun ac c ->
      if List.mem ys c ~equal:check_equal then ac
      else c :: ac)

module Render = struct

  type t = {
      view : View.t;
      arts : (Bap_artifact.kind * artifact) String.Map.t;
      outp : string;
    }

  let create view output = {
      view;
      arts = Map.empty (module String);
      outp = output;
    }

  let update t (kind,arti) =
    {t with arts =
      Map.set t.arts (Artifact.name arti) (kind,arti) }

  let get t name = Map.find t.arts name

  let run t =
    let artis = Map.data t.arts |> List.map ~f:snd in
    let doc = Template.render t.view artis in
    Out_channel.with_file t.outp
      ~f:(fun ch -> Out_channel.output_string ch doc)

end

let update_time arti checks time =
  List.fold checks ~init:arti
    ~f:(fun arti c -> Artifact.with_time arti c time)

let map_of_alist ~init xs =
  List.fold ~init xs ~f:(fun m conf -> Map.set m (Confirmation.id conf) conf)

let read_confirmations path =
  let confs = In_channel.with_file path ~f:Read.confirmations in
  List.fold confs ~init:(Map.empty (module String))
    ~f:(fun m (name,confs) ->
        Map.update m name ~f:(function
            | None -> map_of_alist ~init:Incident_id.Map.empty confs
            | Some confs' -> map_of_alist ~init:confs' confs))

let check_mem checks c =
  List.mem checks c ~equal:(fun c c' -> Incident_kind.compare c c' = 0)

let confirm confirmations arti kinds =
  match Map.find confirmations (Artifact.name arti) with
  | None -> arti
  | Some confirmed ->
     Map.fold confirmed ~init:arti ~f:(fun ~key:id ~data:conf arti ->
         match Artifact.find arti id with
         | Some (inc,st) ->
            let status = Confirmation.validate conf (Some st) in
            Artifact.update arti inc status

         | None ->
            match Confirmation.validate conf None with
            | False_neg as status ->
               let inc = Incident.create
                           (Confirmation.locations conf)
                           (Confirmation.incident_kind conf)  in
               Artifact.update arti inc  status
            | _ -> arti)



let run_artifact tool confirmed arti kind recipe =
  printf "running %s %s\n%!" (Artifact.name arti) (Recipe.name recipe);
  let checks = Artifact.checks arti in
  let recipe = Bap_artifact.run_recipe tool arti kind recipe in
  let time = Recipe.time_taken recipe in
  let incs = "incidents" in
  if Sys.file_exists incs then
    let incs = In_channel.with_file incs ~f:Read.incidents in
    let arti = List.fold incs ~init:arti ~f:(fun a i -> Artifact.update a i Undecided) in
    let checks = check_diff (Artifact.checks arti) checks in
    let arti = update_time arti checks time  in
    confirm confirmed arti checks
  else arti

let need_all names =
  let names = List.map ~f:String.lowercase names in
  List.mem names "all" ~equal:String.equal

let recipes_of_names names =
  let recipes = Recipe.list () in
  if need_all names then recipes
  else
    List.filter recipes
      ~f:(fun r -> List.mem names (Recipe.name r) ~equal:String.equal)

let run tool render confirmed name recipes =
  let recipes = recipes_of_names recipes in
  match Render.get render name with
  | None -> render
  | Some (kind,arti) ->
    List.fold ~init:(render,arti) recipes ~f:(fun (render,arti) reci ->
        let arti = run_artifact tool confirmed arti kind reci in
        let render = Render.update render (kind,arti) in
        Render.run render;
        render,arti) |> fst

let default_view = View.create ()


let run_artifacts tool ?(view=default_view) confirmed out artis recipes =
  let render =
    List.fold artis
      ~init:(Render.create view out) ~f:(fun r name ->
          match Bap_artifact.find name with
          | None ->
            eprintf "didn't find artifact %s, skipping ... \n" name;
            r
          | Some a -> Render.update r a) in
  ignore @@
  List.fold artis
    ~init:render ~f:(fun render name -> run tool render confirmed name recipes)

let parse_path = function
  | [Sexp.Atom x] -> Some x
  | _ -> None

let parse_actions = function
  | [Sexp.Atom target; Sexp.List recipes] ->
    let recipes = List.map recipes ~f:Sexp.to_string in
    Some (target,recipes)
  | [Sexp.Atom target; Sexp.Atom recipe] ->
    Some (target,[recipe])
  | _ -> None

let read_schedule acc path =
  let rec read acc = function
    | [] -> acc
    | Sexp.List data :: xs ->
      let acc = match parse_actions data with
        | None -> acc
        | Some d -> d :: acc in
      read acc xs
    | _ :: xs -> read acc xs in
  let sexps =
    In_channel.with_file path ~f:Sexp.input_sexps in
  read [] sexps |> List.rev

let run_schedule tool ?(view=default_view) confirmed out path =
  let acts = read_schedule [] path  in
  let render =
    List.fold acts
      ~init:(Render.create view out) ~f:(fun r (name,recipes) ->
          match Bap_artifact.find name with
          | None -> r
          | Some a -> Render.update r a) in
  ignore @@
  List.fold acts
    ~init:render
    ~f:(fun render (name,recipes) ->
      run tool render confirmed name recipes)

let check_toolkit tool =
  let tag = Tool.tag tool in
  let image = Tool.name tool in
  match Docker.get_image ?tag image with
  | Ok () -> ()
  | Error _ ->
     eprintf "can't detect/pull bap-toolkit, exiting ... ";
     exit 1

let of_incidents_file ?(view=default_view) output filename =
  let incidents = In_channel.with_file filename ~f:Read.incidents in
  let artifact = Artifact.create filename in
  let artifact = List.fold incidents ~init:artifact ~f:(fun a i ->
      Artifact.update a i Undecided) in
  Out_channel.with_file output ~f:(fun ch ->
      Out_channel.output_string ch @@
        Template.render view [artifact])

module O = struct

  type t = {
    schedule  : string option;
    artifacts : string list;
    recipes   : string list;
    confirms  : string option;
    output    : string;
    of_incs   : string option;
    tool      : string option;
    view      : string option;
  } [@@deriving fields]


  let create a b c d e f g h = Fields.create a b c d e f g h

end

open Cmdliner

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
  `P "A frontend to the whole bap and docker infrastructures,
        that hides all the complexity under the hood: no bap
        installation required, no manual pulling of docker
        images needed.";

  `P  "It allows easily to run
        the various of checks against the various of artifacts
        and get a frendly HTML report with all the incidents found.";

]

let info = Term.info ~man ~doc "bap-tookit"

let schedule =
  let doc = "creates a schedule of artifacts and recipes to run
             from a provided file, that contains s-expressions in
             the form:
             (artifact1 (recipe1 recipe2))
             (artifact2 recipe1)
             (artifact3 all)
             ... " in
  Arg.(value & opt (some string) None & info ~doc ["schedule"])

let strings = Arg.(list string)

let artifacts =
  let doc = "A comma-separated list of artifacts to check.
             Every artifact is either a file in the system
             or a TAG from binaryanalysisplatform/bap-artifacts
             docker image" in
  Arg.(value & opt strings [] & info ["artifacts"] ~doc)

let recipes =
  let doc = "list of recipes to run. A special key $(i,all)
             can be used to run all the recipes" in
  Arg.(value & opt strings [] & info ["recipes"] ~doc)

let confirms =
  let doc = "file with confirmations" in
  Arg.(value & opt (some non_dir_file) None & info ["confirmations"] ~doc)

let output =
  let doc = "file with results" in
  Arg.(value & opt string "results.html" & info ["output"] ~doc)

let list_recipes =
  let doc = "prints the list of available recipes and exits" in
  Arg.(value & flag & info ["list-recipes"] ~doc)

let list_artifacts =
  let doc = "prints list of available artifacts and exits" in
  Arg.(value & flag & info ["list-artifacts"] ~doc)

let of_incidents =
  let doc = "create a report from file with incidents" in
  Arg.(value & opt (some non_dir_file) None & info ["of-incidents"] ~doc)

let tool =
  let doc = "Use a tool other then binaryanalysisplatform/bap-toolkit.
             Tags could be fed as expected, with ':' separator" in
  Arg.(value & opt (some string) None & info ["tool"] ~doc)

let view =
  let doc = "use a view file with view for rendering incidents" in
  Arg.(value & opt (some non_dir_file) None & info ["view"] ~doc)

let is_specified opt ~default =
  Cmdliner.Term.eval_peek_opts opt |>
  fst |> Option.value ~default

let print_recipes_and_exit tool =
  let recipes = Recipe.list ~tool ()  in
  List.iter recipes ~f:(fun r ->
      printf "%-32s %s\n" (Recipe.name r) (Recipe.description r));
  exit 0

let print_artifacts_and_exit () =
  let images = Docker.available_tags Bap_artifact.image in
  List.iter images ~f:(fun tag -> printf "%s\n" tag);
  exit 0

let main o print_recipes print_artifacts =
  let open O in
  let tool = match o.tool with
    | None -> Tool.default
    | Some name ->
       match Tool.of_string name with
       | Ok tool -> tool
       | Error er ->
          eprintf "%s\n" @@ Error.to_string_hum er;
          exit 1 in
  check_toolkit tool;
  if print_recipes   then print_recipes_and_exit tool;
  if print_artifacts then print_artifacts_and_exit ();
  let confirmed = match o.confirms with
    | None -> Map.empty (module String)
    | Some path -> read_confirmations path in
  let view = match o.view with
    | None -> None
    | Some f -> Some (View.of_file f) in
  match o.schedule, o.of_incs with
  | Some sch, _ -> run_schedule ?view tool confirmed o.output sch
  | _, Some file -> of_incidents_file ?view o.output file
  | _ -> run_artifacts ?view tool confirmed o.output o.artifacts o.recipes

let o =
  Term.(const O.create
        $schedule
        $artifacts
        $recipes
        $confirms
        $output
        $of_incidents
        $tool
        $view)

let _ = Term.eval (Term.(const main $o $list_recipes $list_artifacts), info)

(*
TODO: document everything
TODO: install view file somewhere ?
TODO: there is a bug when in infering a size of an artifact
TODO: remove incidents file on exit
TODO: find a way to limit time?
TODO: add something like a dump to (sexp?) file to render later
TODO: add dump to database as bin_io of artifacts. here should be some
      options: dump-db, update-db, render-db
*)
