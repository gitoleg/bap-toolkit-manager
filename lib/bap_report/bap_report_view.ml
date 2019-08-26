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
  Hashtbl.update t (Incident_kind.to_string k) ~f:(function
      | None -> [info]
      | Some xs -> info :: xs)

let find_info t k ~f =
  match Hashtbl.find t (Incident_kind.to_string k) with
  | None -> None
  | Some infos -> List.find_map infos ~f

let name t kind =
  match find_info t kind ~f:(function
            | Alias a -> Some a
            | _ -> None) with
  | None -> Incident_kind.to_string kind
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
  let cols = find_info t kind ~f:(function
      | Tab cols -> Some cols
      | _ -> None) in
  let cols = match cols with
    | None ->[Name;Addr]
    | Some cols -> cols in
  List.rev @@
    List.fold cols ~init:[] ~f:(fun acc -> function
        | Path n ->
           (List.rev (List.take (Incident.path inc) n)) @ acc
        | Name -> name t kind :: acc
        | Addr -> Addr.to_string (Incident.addr inc) :: acc
        | Locs ->
           let addrs = Locations.addrs (Incident.locations inc) in
           List.rev_map ~f:Addr.to_string addrs @ acc)
