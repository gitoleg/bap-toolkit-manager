open Core_kernel
open Format

module Msg = Bap_report_io.Msg

type status =
  | Running
  | Finished
  | Errored
[@@deriving sexp]

type task = {
    name   : string;
    status : status;
    index  : int;
    elapsed  : float;
    incidents : int;
}

let t = String.Table.create ()
let counter = ref 0
let timer = ref 0.0
let enabled = ref false

let string_of_status s =
  Sexp.to_string (sexp_of_status s)

module Ansi = struct
  let pp_pos ppf (x,y) = fprintf ppf "\027[%i;%iH%!" y x
  let pp_clear ppf = fprintf ppf "\027[2J%!"
  let pp_bold ppf = fprintf ppf "\027[1m"
  let pp_norm ppf = fprintf ppf "\027[0m"
end

let to_hms t =
  let open Float in
  let minute = 60. in
  let hour = 60. * minute in
  let day = 24. * hour in
  let secs  = mod_float t minute in
  let mins  = mod_float (t - secs) hour in
  let hours = mod_float (t - mins)  day in
  let (%:) x base = to_int (x / base) in
  let h,m,s=(hours %: hour, mins %: minute, secs %: 1.) in
  sprintf "%02d:%02d:%02d" h m s

let pp fmt = fprintf err_formatter fmt

let enable () =
  enabled := true;
  timer := Unix.gettimeofday ();
  pp "%t%a" Ansi.pp_clear Ansi.pp_pos (1,1)

let finish () =
  if !enabled then
    let len = Hashtbl.length t in
    pp "%a" Ansi.pp_pos (1,1 + 1 + len)

let () = at_exit finish

let update_time () =
  let old = !timer in
  timer := Unix.gettimeofday ();
  let since = !timer -. old in
  let keys = Hashtbl.keys t in
  List.iter keys ~f:(fun key ->
      let data = Hashtbl.find_exn t key in
      match data.status with
      | Finished | Errored -> ()
      | _ ->
         let elapsed = data.elapsed +. since in
         Hashtbl.set t key {data with elapsed })

let string_of_time tm =
  let open Unix in
  let local = localtime tm in
  sprintf "%02d:%02d:%02d"
            local.tm_hour local.tm_min local.tm_sec

let new_task name =
  incr counter;
  let index = !counter in
  Hashtbl.set t name
    {name;index; status = Running; elapsed = 0.0; incidents = 0}


let update = function
  | `Job_started name -> new_task name
  | `Tick -> update_time ()
  | `Job_incidents (name,incs) ->
     Hashtbl.change t name ~f:(function
         | None -> None
         | Some t -> Some {t with incidents=incs})
  | `Job_finished name ->
     Hashtbl.change t name ~f:(function
         | None -> None
         | Some t -> Some {t with status = Finished})
  | `Job_errored name ->
     Hashtbl.change t name ~f:(function
         | None -> None
         | Some t -> Some {t with status = Errored})

let cmp x y = Int.compare x.index y.index

let render msg =
  update msg;
  let items = List.sort (Hashtbl.data t) ~compare:cmp in
  pp "%t%a%-15s%-11s%-36s%s\n"
    Ansi.pp_bold Ansi.pp_pos (1,1) "Time elapsed" "Status" "Job" "Incidents";
  pp "%t" Ansi.pp_norm;
  List.iter items ~f:(fun {name;index;status;elapsed; incidents} ->
      pp "%a%s       %-10s %-35s %d"
        Ansi.pp_pos (1,1 + index)
        (to_hms elapsed)
        (string_of_status status)
        name
        incidents)
