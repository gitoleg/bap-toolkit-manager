open Core_kernel
open Bap_report.Std

module Helper = struct
  open Bin_prot

  let write ch bin_writer t =
    let buf = Utils.bin_dump ~header:true bin_writer t in
    Out_channel.output_string ch (Bigstring.to_string buf);
    Out_channel.flush ch

  let read_from_channel chan buf ~pos ~len =
    let s = Bytes.create len in
    match In_channel.really_input chan ~buf:s ~pos:0 ~len with
    | None -> raise End_of_file
    | Some () ->
      Bigstring.From_bytes.blito ~src:s ~dst:buf ~dst_pos:pos ()

  let read ch bin_reader =
    try
      let read = read_from_channel ch in
      let value = Utils.bin_read_stream ~read bin_reader in
      Some (Ok value)
    with
    | End_of_file -> None
    | exn -> Some (Error (Error.of_exn exn))
end

module Artifacts = struct

  let write ch arti = Helper.write ch Artifact.bin_writer_t arti
  let read ch = Helper.read ch Artifact.bin_reader_t

  let update_db m artis =
    let try_update m a =
      let name = Artifact.name a in
      match Map.find m name with
      | None -> Ok (Map.set m name a)
      | Some a' ->
        match Artifact.merge a a' with
        | Some a -> Ok (Map.set m name a)
        | None ->
          Or_error.errorf "can't update db, got a conflict for %s"
            name in
    let rec update m = function
      | [] -> Ok m
      | a :: artis ->
        match try_update m a with
        | Ok m -> update m artis
        | Error er -> Error er in
    update m artis

  let read file =
    let rec loop acc ch =
      match read ch with
      | Some Ok a -> loop (a :: acc) ch
      | Some Error _ -> loop acc ch
      | None -> List.rev acc in
    let artis = In_channel.with_file file
        ~f:(fun ch -> loop [] ch) in
    let m = Map.empty (module String) in
    match update_db m artis with
    | Error er ->
      eprintf "can't rad db: %s\n" (Error.to_string_hum er);
      []
    | Ok m -> Map.data m

  let save file artifacts =
    Out_channel.with_file file
      ~f:(fun ch -> List.iter artifacts ~f:(write ch))

  let dump ?(update=false) file artifacts =
    let old =
      if update then read file
      else [] in
    let m = Map.empty (module String) in
    match Or_error.(update_db m old >>= fun m -> update_db m artifacts) with
    | Error er ->
      let tmp = Filename.temp_file ~temp_dir:(Sys.getcwd ()) "dump" "bin" in
      eprintf "%s, will store to %s\n" (Error.to_string_hum er) tmp;
      save tmp artifacts
    | Ok m -> save file (Map.data m)

end

module Msg = struct
  open Bin_prot

  type t =
    | Job_started of string
    | Job_finished of string
  [@@deriving bin_io]

  let write ch msg =
    Helper.write ch bin_writer_t msg

  let read ch =
    match Helper.read ch bin_reader_t with
    | Some (Ok msg) -> Some msg
    | _ -> None

end
