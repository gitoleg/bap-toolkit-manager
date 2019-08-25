open Core_kernel
open Bap_report_types


type tab =
  | Path of int
  | Name of string
  | Addr of addr
  | Locs of addr list

type view =
  | Web of string
  | Tab of tab
  | Alias of string


type t = view list String.Table.t

let create () = Hashtbl.create (module String)

let update t name view =
  Hashtbl.update t name ~f:(function
      | None -> [view]
      | Some xs -> view :: xs)


let kind inc = Incident.kind inc |> Incident.string_of_kind

let find_view t inc ~f =
  match Hashtbl.find t (kind inc) with
  | None -> None
  | Some views -> List.find_map views ~f

let name t inc =
  match find_view t inc ~f:(function
            | Alias a -> Some a
            | _ -> None) with
  | None -> kind inc
  | Some a -> a

let path t inc =
  find_view t inc ~f:(function
      | Path p -> Some p
      | _ -> None)

let web t inc =
  find_view t inc ~f:(function
      | Web w -> Some w
      | _ -> None)
