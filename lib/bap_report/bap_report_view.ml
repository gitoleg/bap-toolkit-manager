open Core_kernel
open Bap_report_types


type col =
  | Path of int (* deep  *)
  | Name
  | Addr
  | Locs

type info =
  | Web of string
  | Tab of col list
  | Alias of string

type t = info list String.Table.t

let create () = Hashtbl.create (module String)


let update t k info =
  Hashtbl.update t (Incident.string_of_kind k) ~f:(function
      | None -> [info]
      | Some xs -> info :: xs)

let find_info t k ~f =
  match Hashtbl.find t (Incident.string_of_kind k) with
  | None -> None
  | Some infos -> List.find_map infos ~f

let name t kind =
  match find_info t kind ~f:(function
            | Alias a -> Some a
            | _ -> None) with
  | None -> Incident.string_of_kind kind
  | Some a -> a

let path t inc =
  match find_info t (Incident.kind inc) ~f:(function
      | Path p -> Some p
      | _ -> None) with
  | None -> []
  | Some n ->
     List.take (Incident.path inc) n

let web t inc =
  find_info t inc ~f:(function
      | Web w -> Some w
      | _ -> None)


let data t inc =
  let kind = Incident.kind inc in
  find_info t kind ~f:(function
      | Tab t -> Some t
      | _ -> None)  |> function
  | None -> [ Addr.to_string (Incident.addr inc) ]
  | Some cols ->
     List.rev @@
     List.fold cols ~init:[] ~f:(fun acc -> function
         | Path n ->
            (List.rev (List.take (Incident.path inc) n)) @ acc
         | Name -> name t kind :: acc
         | Addr -> Addr.to_string (Incident.addr inc) :: acc
         | Locs ->
            List.rev_map ~f:Addr.to_string (Incident.locations inc) @ acc)
