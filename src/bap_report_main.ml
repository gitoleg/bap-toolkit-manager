open Core_kernel
open Bap_report.Std
open Bap_report_options

module Run = Bap_report_run

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
      let cmd = sprintf "find / -type f -name /artifact" in
      match Image.run image cmd with
      | None | Some "" ->
        can't_find tag "no such file in image";
        false
      | _ -> true

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

let print_artifacts_and_exit () =
  let images = Image.tags (Lazy.force Bap_artifact.image) in
  List.iter images ~f:(fun tag -> printf "%s\n" tag);
  exit 0

let main o print_recipes print_artifacts =
  if print_artifacts then print_artifacts_and_exit ();
  let store = Option.map o.store ~f:(fun x -> x, o.update) in
  let t = Run.create ?confirmations:o.confirms ?store ~output:o.output o.context in
  match o.mode with
  | From_incidents incs -> Run.of_incidents_file t incs
  | From_stored db -> Run.of_db t db
  | Run_artifacts tasks ->
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
     Run.run t tasks 2

let _ =
  let open Cmdliner in
  let open Bap_report_cmd_terms in
  Term.eval (Term.(const main $options $list_recipes $list_artifacts), info)

(* TODO: install view file somewhere
   TODO: maybe rewise all the directories/archives creation:
         maybe make more distiguive names or place everything in the
         temp dir
   TODO: check limits once more again *)
