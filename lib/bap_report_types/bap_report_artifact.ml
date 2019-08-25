open Core_kernel
open Bap_report_common

module Incident = Bap_report_incident
type incident = Incident.t
type incident_kind = Bap_report_incident.kind  [@@deriving bin_io, compare, sexp]

module Kind = struct
  module Cmp = struct
    type t = incident_kind [@@deriving bin_io, compare, sexp]
    include Comparator.Make(struct
        type nonrec t = t [@@deriving bin_io, compare, sexp]
      end)
  end
  include Cmp

  module Map = Map.Make(Cmp)
  module Set = Set.Make(Cmp)

end

type t = {
  name : string;
  size : int option;
  data : status Incident.Map.t Kind.Map.t;
  time : float Kind.Map.t;
}

module Size = struct

  let to_string_hum bytes =
    let kbytes = bytes / 1024 in
    let mbytes = kbytes / 1024 in
    match mbytes, kbytes, bytes with
    | 0,0,b -> sprintf "%d bytes" b
    | 0,k,_ -> sprintf "%dK" k
    | m,_,_ -> sprintf "%dM" m

end

module Time = struct

  let to_string_hum secs =
    let secs = int_of_float secs in
    let hours = secs / 3600 in
    let minut = (secs - hours * 3600) / 60 in
    let secnd  = (secs - hours * 3600 - minut * 60) in
    sprintf "%02d:%02d:%02d" hours minut secnd

end


let create ?size name  = {
  name; size; data = Kind.Map.empty; time = Kind.Map.empty;
}

let name t = t.name
let size t = t.size

let size_hum t =
  match t.size with
  | None -> None
  | Some x -> Some (Size.to_string_hum x)

let checks t = Map.to_alist t.data |> List.map ~f:fst

let update t incident status =
  let kind = Incident.kind incident in
  {t with
   data =
     Map.update t.data kind ~f:(function
         | None -> Incident.Map.singleton incident status
         | Some res ->
            Map.update res incident ~f:(function
               | None -> status
               | Some status' when status' = Undecided -> status
               | Some status' -> status'))}

let find_result t kind =
  match Map.find t.data kind with
  | None -> []
  | Some x -> Map.to_alist x

let with_size t size = {t with size=Some size}
let with_time t kind time = {t with time = Map.set t.time ~key:kind ~data:time}

let time t kind = Map.find t.time kind

let time_hum t kind =
  match time t kind with
  | None -> None
  | Some time -> Some (Time.to_string_hum time)

let summary t kind =
  match Map.find t.data kind with
  | None -> {false_pos=0; false_neg=0; confirmed=0; undecided=0;}
  | Some x ->
    let false_pos,false_neg,confirmed,undecided =
      Map.fold x ~init:(0,0,0,0)
        ~f:(fun ~key:_ ~data:status (fp,fn,cn,un) ->
            match status with
            | False_pos -> fp + 1, fn, cn, un
            | False_neg -> fp, fn + 1, cn, un
            | Confirmed -> fp, fn, cn + 1, un
            | Undecided -> fp, fn, cn, un + 1) in
    {false_pos; false_neg; confirmed; undecided;}

let incidents ?kind t =
  match kind with
  | None ->
     let incs = Map.data t.data |> List.map ~f:Map.to_alist in
     List.concat incs
  | Some kind ->
     match Map.find t.data kind with
     | None -> []
     | Some incs -> Map.to_alist incs
