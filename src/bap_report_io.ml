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
  open Or_error

  let write ch arti = Helper.write ch Artifact.bin_writer_t arti
  let read ch = Helper.read ch Artifact.bin_reader_t

  let read file =
    let rec loop acc ch =
      match read ch with
      | Some a -> loop (a :: acc) ch
      | None -> List.rev acc in
    In_channel.with_file file ~f:(fun ch -> loop [] ch) |>
    Result.all

  let dump file artifacts =
    Out_channel.with_file file
      ~f:(fun ch -> List.iter artifacts ~f:(write ch))

end

module Msg = struct
  open Bin_prot

  type job_id = string [@@deriving bin_io, sexp]

  type t = [
    | `Job_started of job_id
    | `Job_finished of job_id
    | `Job_errored  of job_id * string
    | `Job_incidents of job_id * int
    | `Tick
  ] [@@deriving bin_io, sexp]

  let write ch msg =
    Helper.write ch bin_writer_t msg

  let read ch =
    match Helper.read ch bin_reader_t with
    | Some (Ok msg) -> Some msg
    | _ -> None

end
