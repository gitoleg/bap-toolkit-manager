open Core_kernel
open Cmdliner
open Bap_report.Std

module Config = Bap_report_config
module Cmd = Bap_report_cmd_terms

open Bap_report_scheduled

type mode =
  | From_incidents of string
  | From_stored    of string
  | Run_artifacts  of (artifact list * recipe list) list

type t = {
  mode       : mode;
  context    : Job.ctxt;
  confirms   : string option;
  output     : string;
  store      : string option;
  update     : bool;
  jobs       : int;
} [@@deriving fields]

module Bap_artifact = struct

  let image =
    Lazy.from_fun (fun () ->
        Image.of_string_exn "binaryanalysisplatform/bap-artifacts")

  let with_tag tag = Image.with_tag (Lazy.force image) tag

  let can't_find tag reason = eprintf "can't find %s: %s\n" tag reason

  let artifact_exists tag =
    let image = with_tag tag in
    match Image.get image with
    | Error er ->
      can't_find tag (Error.to_string_hum er);
      false
    | Ok () ->
      let cmd = {|[ -f /artifact ] && echo "Found" || echo "Not found"|} in
      match Image.run ~interactive:true image cmd with
      | Some x when String.strip x = "Found" -> true
      |  _ ->
        can't_find tag "no such file in image";
        false


  let file_of_name name =
    if Sys.file_exists name then Some (File.create name)
    else if artifact_exists name then
      Some (File.create ~image:(with_tag name) "/artifact")
    else None

  let find name =
    match file_of_name name with
    | None -> None
    | Some file ->
      Some (Artifact.create ~file name)

end

let create_artifact name =
  match Bap_artifact.find name with
  | None ->
    eprintf "didn't find artifact %s, skipping ... \n" name;
    None
  | a -> a


let find_recipe tool r =
  let name = Cmd.requested_name r in
  match Tool.find_recipe tool name with
  | None -> Or_error.errorf "can't find recipe %s\n" name
  | Some recipe ->
    let recipe =
      List.fold (Cmd.requested_pars r) ~init:recipe
        ~f:(fun recipe (p,v) ->
            Recipe.add_parameter recipe p v) in
    Ok recipe

let all_recipes tool = Tool.recipes tool

let create_recipes tool recipes =
  match List.find recipes ~f:(fun r -> Cmd.requested_name r = "all") with
  | Some _ -> Ok (all_recipes tool)
  | None ->
    Result.all @@ List.map recipes ~f:(find_recipe tool)

let create_recipes config tool recipes =
  Or_error.map (create_recipes tool recipes) ~f:(fun rs ->
      List.map rs ~f:(Config.provide_kinds config))

let read_config = function
  | None -> Config.empty
  | Some f -> Config.read f

let print_recipes_and_exit tool =
  let recipes = Tool.recipes tool in
  List.iter recipes ~f:(fun r ->
      printf "%-32s %s\n" (Recipe.name r) (Recipe.description r));
  exit 0

let print_bap_version_and_exit tool =
  printf "bap version: %s\n" @@Tool.bap_version tool;
  exit 0

let print_artifacts_and_exit () =
  let images = Image.tags (Lazy.force Bap_artifact.image) in
  List.iter images ~f:(fun tag -> printf "%s\n" tag);
  exit 0

let print_and_exit tool recipes version artifacts =
  if recipes then print_recipes_and_exit tool;
  if version then print_bap_version_and_exit tool;
  if artifacts then print_artifacts_and_exit ()

let create tool mode ctxt print_recipes print_bap_ver print_artis conf out store update j =
  let tool = tool () in
  print_and_exit tool print_recipes print_bap_ver print_artis;
  let mode = mode tool in
  let ctxt = ctxt tool in
  Fields.create mode ctxt conf out store update j

let check_if_nothing_to_do xs =
  let check what is_empty =
    if is_empty then
      let () = eprintf
        "there nothing I can do: the list of %s is empty\n" what in
      exit 1 in
  let names = List.map xs ~f:fst in
  let recipes = List.map xs ~f:snd in
  check "artifacts" @@ List.for_all names ~f:List.is_empty;
  check "recipes"   @@ List.for_all recipes ~f:List.is_empty

let make_run tool = function
  | Error er ->
    eprintf "%s\n" @@ Error.to_string_hum er;
    exit 1
  | Ok tasks ->
    let tasks,_ =
      List.fold tasks ~init:([],Map.empty (module String))
        ~f:(fun (tasks,known) (artis,recipes) ->
            let artis,known =
              List.fold artis ~init:([],known) ~f:(fun (artis,known) name ->
                  match Map.find known name with
                  | Some a -> a :: artis, known
                  | None -> match create_artifact name with
                    | None -> artis, known
                    | Some a ->
                      a :: artis,
                      Map.set known name a) in
            (List.rev artis, recipes) :: tasks, known) in
     check_if_nothing_to_do tasks;
     Run_artifacts tasks

let infer_mode config of_schedule of_file of_incidents artifacts recipes tool =
  let (>>=) = Or_error.(>>=) in
  let config = read_config config in
  match of_schedule, of_file, of_incidents with
  | Some f,_,_ ->
    let acts = Bap_report_scheduled.of_file f in
    let rs =
      List.fold acts
        ~init:(Ok [])
        ~f:(fun acc s ->
            acc >>= fun acc ->
            create_recipes config tool s.recipes >>= fun rs ->
            Ok (([s.artifact], rs) :: acc)) in
    make_run tool rs
  | _,Some f,_ -> From_stored f
  | _,_,Some f -> From_incidents f
  | _ ->
    let rs =
      Ok (List.concat artifacts) >>= fun artis ->
      create_recipes config tool (List.concat recipes) >>= fun recipes ->
      Ok [artis,recipes] in
    make_run tool rs

let tool_of_string s () =
  let tool = match s with
    | "host" -> Tool.host ()
    | s ->
      Or_error.(Image.of_string s >>= Tool.of_image) in
  match tool with
  | Ok t -> t
  | Error er ->
    eprintf "can't find or create tool %s: %s" s (Error.to_string_hum er);
    exit 1

let context limits verbose tool =
  let limit = List.fold limits
      ~init:Limit.empty ~f:(fun l (n,q) -> Limit.add l n q) in
  Job.context ~verbose ~limit tool

open Cmd
open Term

let options =
  let ctxt = const context $limits $verbose in
  let mode = const infer_mode
                  $config $schedule
                  $of_file $of_incidents
                  $artifacts $recipes in
  let tool = const tool_of_string $tool in
  const create $tool $mode $ctxt
  $list_recipes $bap_version $list_artifacts
  $confirms $report
  $store $update $jobs
