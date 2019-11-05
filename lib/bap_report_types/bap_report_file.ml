open Core_kernel
open Bap_report_utils

module Image = Bap_report_image

type image = Image.t [@@deriving bin_io, compare, sexp]

type t = image option * string
[@@deriving bin_io, compare, sexp]

let create ?image path = image, path

let image = fst
let path = snd

let size (image,path) = match image with
    | None ->
      let s = Unix.stat path in
      Some Unix.(s.st_size)
    | Some image ->
      match Image.run image (sprintf "stat -c%%s %s" path) with
      | Error _ -> None
      | Ok s ->
        let s = String.strip s in
        try Some (int_of_string s)
        with _ -> None

let same_image im im' = match im,im' with
  | None, None -> true
  | Some im, Some im' ->
    String.equal (Image.to_string im) (Image.to_string im')
  | _ -> false

let same_path = String.equal

let equal (image, path) (image',path') =
  same_image image image' && same_path path path'
