open Core_kernel
open Format

module Msg = Bap_report_io.Msg

type task = {
    name   : string;
    status : string;
    index  : int;
    elapsed  : float;
}

let t = String.Table.create ()
let counter = ref 0
let timer = ref (Unix.gettimeofday ())


module Ansi = struct
  let pp_pos ppf (x,y) = fprintf ppf "\027[%i;%iH%!" y x
  let pp_clear ppf = fprintf ppf "\027[2J%!"
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

let start () =
  pp "%t%a" Ansi.pp_clear Ansi.pp_pos (1,1)

let finish () =
  let len = Hashtbl.length t in
  pp "%a" Ansi.pp_pos (1,1 + len)

let () = start ()
let () = at_exit finish

let update_time () =
  let old = !timer in
  timer := Unix.gettimeofday ();
  let since = !timer -. old in
  let keys = Hashtbl.keys t in
  List.iter keys ~f:(fun key ->
      let data = Hashtbl.find_exn t key in
      if data.status = "finished" then ()
      else
        let elapsed = data.elapsed +. since in
        Hashtbl.set t key {data with elapsed })

let string_of_time tm =
  let open Unix in
  let local = localtime tm in
  sprintf "%02d:%02d:%02d"
            local.tm_hour local.tm_min local.tm_sec

let update = function
  | Msg.Job_started name ->
     let index = !counter in
     incr counter;
     Hashtbl.set t name {name;index; status = "running"; elapsed = 0.0}
  | Msg.Tick -> update_time ()
  | Msg.Job_finished name ->
     Hashtbl.change t name ~f:(function
         | None -> None
         | Some t -> Some {t with status = "finished"})

let cmp x y = Int.compare x.index y.index

let render msg =
  update msg;
  let items = List.sort (Hashtbl.data t) ~compare:cmp in
  List.iter items ~f:(fun {name;index;status;elapsed} ->
      pp "%a%s   %-10s %s" Ansi.pp_pos (1,1 + index) (to_hms elapsed) status name)
