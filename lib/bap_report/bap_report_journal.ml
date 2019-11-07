open Core_kernel
open Bap_report_types
open Bap_report_utils
open Bap_report_read
open Bap_report_env

type t = string  [@@deriving bin_io,compare,hash,sexp]

let equal = String.equal

let fullname x = sprintf "%s.tgz" x

let of_workdir x =
  let full = fullname x in
  if Sys.file_exists full then
    Sys.remove full;
  x

let write_cmd t =
  let dir = Filename.basename t in
  sprintf "tar czf %s.tgz %s" dir dir

let tar_list tar =
  match cmd "tar -ztf %s" tar with
  | Error _ -> []
  | Ok s ->
    List.filter_map ~f:(fun x ->
        let x = String.strip x in
        if String.is_empty x then None
        else Some x) @@
    String.split ~on:'\n' s

let tar_exists ~file tar =
  List.exists (tar_list tar) ~f:(fun s -> String.equal s file)

let read_tar ?target_dir target_file t read =
  let tar = fullname t in
  if Sys.file_exists tar then
    let dir = Filename.remove_extension tar in
    let path = match target_dir with
      | None -> sprintf "%s/%s" dir target_file
      | Some dir' -> sprintf "%s/%s/%s" dir dir' target_file in
    if tar_exists ~file:path tar then
      let _ = cmd "tar xzf %s %s" tar path in
      if Sys.file_exists path then
        let r = read path in
        Sys.remove path;
        Option.iter target_dir ~f:(fun dir' -> Unix.rmdir (sprintf "%s/%s" dir dir'));
        Unix.rmdir dir;
        r
      else None
    else None
  else None

let incidents t =
  let read f = Some (In_channel.with_file f ~f:Bap_report_read.incidents) in
  match read_tar incidents_file t read with
  | None -> []
  | Some incs -> incs

let errors t =
  match read_tar ~target_dir:"log" "log" t Log.of_file with
  | None -> []
  | Some log -> Log.errors log

let time t =
  match read_tar mytime t Time.of_file with
  | None -> None
  | Some tm -> Time.elapsed tm

let incidents' ?(bookmark=0L) t =
  let from_file f =
    let ch = In_channel.create f in
    In_channel.seek ch bookmark;
    let incs = Bap_report_read.incidents ch in
    let pos = In_channel.pos ch in
    In_channel.close ch;
    List.length incs, pos in
  let read_from_tar t =
    let read f = from_file f |> Option.some in
    match read_tar incidents_file t read with
    | None -> 0, bookmark
    | Some x -> x in
  try
    from_file (sprintf "%s/%s" t incidents_file)
  with _ -> read_from_tar t

let remove j =
  let full = fullname j in
  if Sys.file_exists full then
    Sys.remove full


module T = struct
  type nonrec t = t [@@deriving bin_io,compare,hash,sexp]
  let module_name = "Bap_report.Std.Journal"
  let to_string x = x
  let of_string x = x
end

include Identifiable.Make(T)
