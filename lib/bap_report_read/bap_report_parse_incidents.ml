open Core_kernel

open Bap_report_types
open Bap_report_read_types

let some = Option.some

let read_sexp ch =
  try Sexp.input_sexp ch |> some
  with _ -> None

open Sexp

let point_of_sexp x = match x with
  | List _ -> None
  | Atom x ->
    match String.split ~on:':' x  with
    | [_; addr] -> Some (Addr.of_string addr)
    | [addr] -> Some (Addr.of_string addr)
    | _ -> None

let trace_of_sexps xs =
  List.filter_map ~f:point_of_sexp xs

let locs_of_sexps xs =
  List.filter_map xs ~f:(function
      | Atom s -> Some s
      | _ -> None)

let of_sexp s = match s with
  | List (Atom "incident-location" :: List [Atom loc_id; List points] :: _)  ->
    Incident_location (loc_id, trace_of_sexps points) |> some
  | List (Atom "incident" :: List (Atom name :: locs) :: _) ->
     Incident (Incident.kind_of_string name, locs_of_sexps locs) |> some
  | List (Atom "machine-switch" :: List [Atom from; Atom to_ ] :: _ ) ->
    Switch (from,to_) |> some
  | List (Atom "machine-fork" :: List [Atom from; Atom to_ ] :: _ ) ->
    Fork (from,to_) |> some
  | List (Atom "call" :: List (Atom name :: _) :: _ ) ->
    Call name |> some
  | List (Atom "call-return" :: List (Atom name :: _) :: _ ) ->
    Call_return name |> some
  | List (Atom "symbol" :: List [Atom name; Atom addr] :: _) ->
    Symbol (name, Addr.of_string addr) |> some
  | List (Atom "pc-changed" :: Atom addr :: _) ->
    Pc_changed (Addr.of_string addr) |> some
  | _ -> None

let of_sexp s =
  try
    of_sexp s
  with _ -> None

let rec read ch =
  match read_sexp ch with
  | None -> None
  | Some s ->
    match of_sexp s with
    | None -> read ch
    | Some _ as r -> r

let try_sexp f s = Option.try_with (fun () -> f s)

let read_confirmations ch =
  let locs_of_sexp xs =
    let locs = List.map ~f:string_of_sexp xs in
    List.map ~f:Addr.of_string locs in
  let rec confs_of_sexps acc = function
    | [] -> acc
    | List (conf :: inc_kind :: locs) :: confs ->
       let x =
         Option.(try_sexp confirmation_kind_of_sexp conf >>= fun conf ->
                 try_sexp incident_kind_of_sexp inc_kind >>= fun kind ->
                 let locs = locs_of_sexp locs in
                 some @@ Confirmation.create conf kind locs) in
       let acc = match x with
         | None -> acc
         | Some x -> x :: acc in
       confs_of_sexps acc confs
    | _ :: confs -> confs_of_sexps acc confs in
  let rec read acc =
    match read_sexp ch with
    | None -> acc
    | Some (List (Atom name :: List confs :: _ )) ->
       let confs = confs_of_sexps [] confs in
       read ((name, confs) :: acc)
    | _ -> read acc in
  read []
