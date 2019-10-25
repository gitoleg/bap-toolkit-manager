open Core_kernel
open Bap_report.Std

type task = {
  arti : artifact;
  journals : journal list
}

type t = {
  ctxt  : Job.ctxt;
  tasks : task String.Map.t;
  confirmed : confirmation Incident.Id.Map.t String.Map.t;
  output  : string;
  expected : Incident.Kind.Set.t Journal.Map.t;
}

let map_of_alist ~init xs =
  List.fold ~init xs
    ~f:(fun m conf ->
        Map.set m (Confirmation.id conf) conf)

let read_confirmations = function
  | None -> Map.empty (module String)
  | Some path ->
    let confs = In_channel.with_file path ~f:Read.confirmations in
    List.fold confs ~init:(Map.empty (module String))
      ~f:(fun m (name,confs) ->
          Map.update m name ~f:(function
              | None -> map_of_alist ~init:Incident.Id.Map.empty confs
              | Some confs' -> map_of_alist ~init:confs' confs))

let confirm confirmations arti kinds =
  match Map.find confirmations (Artifact.name arti) with
  | None -> arti
  | Some confirmed ->
    let checks = Artifact.checks arti in
    Map.fold confirmed ~init:arti ~f:(fun ~key:id ~data:conf arti ->
        let k = Confirmation.incident_kind conf in
        if not (List.mem checks k ~equal:Incident.Kind.equal)
        then arti
        else
          match Artifact.find arti id with
          | Some (inc,st) ->
            let status = Confirmation.validate conf (Some st) in
            Artifact.update arti inc status
          | None ->
            match Confirmation.validate conf None with
            | False_neg as status ->
              let inc = Incident.create
                  (Confirmation.locations conf)
                  (Confirmation.incident_kind conf)  in
              Artifact.update arti inc status
            | _ -> arti)

let create ctxt output conf =
  let confirmed = read_confirmations conf in
  {
    ctxt;
    tasks = Map.empty (module String);
    output;
    confirmed;
    expected = Map.empty (module Journal);
  }


let new_task arti = {
  arti;
  journals = []
}

let prepare_jobs t xs =
  let find_task t a =
    match Map.find t.tasks (Artifact.name a) with
    | None -> new_task a
    | Some t -> t in
  let update_expected t recipe journal =
    let kinds =
      Recipe.kinds recipe |> Set.of_list (module Incident.Kind) in
    {t with expected = Map.set t.expected journal kinds } in
  List.fold xs ~init:(t,[])
    ~f:(fun init (artis,recipes) ->
        List.fold artis ~init
          ~f:(fun (t,jobs) a -> match Artifact.file a with
              | None -> t,jobs
              | Some file ->
                let task = find_task t a in
                let t,task,jobs =
                  List.fold recipes ~init:(t,task,jobs) ~f:(fun (t,task,jobs) r ->
                      let job = Job.prepare t.ctxt r file in
                      let journal = Job.journal job in
                      let t = update_expected t r journal in
                      let task = {task with journals = journal::task.journals} in
                      t, task, job :: jobs) in
                {t with tasks = Map.set t.tasks (Artifact.name a) task},jobs))

let artifacts ?names t =
  let mem name =
    match names with
    | None -> true
    | Some names -> Set.mem names name in
  Map.fold t.tasks ~init:[] ~f:(fun ~key:name ~data  artis ->
      if mem name then data.arti :: artis
      else artis)

let render ?ready t =
  let doc = Template.render (artifacts ?names:ready t) in
  Out_channel.with_file t.output
    ~f:(fun ch -> Out_channel.output_string ch doc)

let find_by_journal t j =
  Map.data t.tasks |>
  List.find ~f:(fun task ->
      List.exists task.journals ~f:(fun j' -> Journal.equal j j'))

let print_errors job =
  List.iter (Journal.errors @@ Job.journal job) ~f:(eprintf "%s\n")

let missed_kinds expected incidents =
  let happened = List.fold incidents ~init:(Set.empty (module Incident.Kind))
      ~f:(fun kinds inc -> Set.add kinds (Incident.kind inc)) in
  Set.diff expected happened |> Set.to_list

let find_expected t journal =
  match Map.find t.expected journal with
  | Some x -> x
  | None -> Set.empty (module Incident.Kind)

let check_equal x y = compare_incident_kind x y = 0

let check_diff xs ys =
  List.fold xs ~init:[] ~f:(fun ac c ->
      if List.mem ys c ~equal:check_equal then ac
      else c :: ac)

let update_time arti checks = function
  | None -> arti
  | Some time ->
     List.fold checks ~init:arti
       ~f:(fun arti c -> Artifact.with_time arti c time)


let startup_time () =
  let open Unix in
  let t = gettimeofday () |> localtime in
  sprintf "%02d:%02d:%02d" t.tm_hour t.tm_min t.tm_sec


let run_seq t xs =
  let t,jobs = prepare_jobs t xs in
  let ready = Set.empty (module String) in
  List.fold jobs ~init:(t,ready) ~f:(fun (t,ready) j ->
      printf "%s: %s\n%!" (startup_time ()) (Job.name j);
      let j = Job.run t.ctxt j in
      print_errors j;
      let journal = Job.journal j in
      match find_by_journal t journal with
      | None -> t,ready
      | Some task ->
         let incs = Journal.incidents journal in
         let time = Journal.time journal in
         let expected = find_expected t journal in
         let missed = missed_kinds expected incs in
         let checks = Artifact.checks task.arti in
         let arti = List.fold missed ~init:task.arti
                      ~f:(fun arti kind ->
                        let a = Artifact.no_incidents arti kind in
                        match time with
                        | None -> a
                        | Some time -> Artifact.with_time a kind time) in
         let arti = List.fold incs ~init:arti ~f:(fun a i -> Artifact.update a i Undecided) in
         let diff = check_diff (Artifact.checks arti) checks in
         let arti = update_time arti diff time in
         let arti = confirm t.confirmed arti diff in
         let task = { task with arti } in
         let t = {t with tasks = Map.set t.tasks (Artifact.name arti) task} in
         let ready = Set.add ready (Artifact.name arti) in
         render t ~ready;
         t, ready)

let add t arti =
  let task = new_task arti in
  {t with tasks = Map.set t.tasks (Artifact.name arti) task}

let of_incidents_file t filename =
  let name = Filename.remove_extension filename in
  let incidents = In_channel.with_file filename ~f:Read.incidents in
  let artifact = Artifact.create name in
  let artifact = List.fold incidents ~init:artifact
      ~f:(fun a i -> Artifact.update a i Undecided) in
  let artifact = confirm t.confirmed artifact (Artifact.checks artifact) in
  let t = add t artifact in
  render t

let of_db t db =
  let artis = Bap_report_io.read db in
  let t = List.fold artis ~init:t ~f:add in
  render t

let run_threads ctxt ts =
  let run jobs =
    List.iter jobs ~f:(fun j -> ignore @@ Job.run ctxt j) in
  let rec loop acc = function
    | [] -> acc
    | t :: ts ->
       match Unix.fork () with
       | 0 -> run t; exit 0
       | pid -> loop (pid :: acc) ts in
  List.iter (loop [] ts) ~f:(fun pid -> ignore @@ Unix.waitpid [] pid)

let run_parallel t xs n =
  let t,jobs = prepare_jobs t xs in
  let chunks =                  (* WRONG!  *)
    let len = List.length jobs in
    let n' = len / n in
    if n' * n <> len then n' + 1
    else n' in
  let threads = List.chunks_of ~length:chunks jobs in
  run_threads t.ctxt threads;
  let t =
    List.fold jobs ~init:t ~f:(fun t j ->
        printf "%s: %s\n%!" (startup_time ()) (Job.name j);
        let journal = Job.journal j in
        match find_by_journal t journal with
        | None -> t
        | Some task ->
           let incs = Journal.incidents journal in
           let time = Journal.time journal in
           let expected = find_expected t journal in
           let missed = missed_kinds expected incs in
           let checks = Artifact.checks task.arti in
           let arti = List.fold missed ~init:task.arti
                        ~f:(fun arti kind ->
                          let a = Artifact.no_incidents arti kind in
                          match time with
                          | None -> a
                          | Some time -> Artifact.with_time a kind time) in
           let arti = List.fold incs ~init:arti ~f:(fun a i -> Artifact.update a i Undecided) in
           let diff = check_diff (Artifact.checks arti) checks in
           let arti = update_time arti diff time in
           let arti = confirm t.confirmed arti diff in
           let task = { task with arti } in
           {t with tasks = Map.set t.tasks (Artifact.name arti) task}) in
   render t

(* let bind xt f =
 *   match xt with
 *   | 0 -> 0
 *   | pids ->
 *      match Unix.fork () with
 *      | 0 -> f (); exit 0
 *      | pid -> pid
 *
 *
 * let (>>=) = bind
 *
 *
 * let test =
 *   do_some_job >>= fun pid ->
 *   do_some_job' >>= fun pid' ->
 *   Unix.wait [] pid;
 *   Unix.wait [] pid' *)




(* let run_parallel t a b recipes =
 *   let () = printf "run in parallel!\n%!" in
 *   let pid = Unix.fork () in
 *   if pid = 0 then
 *     run t a recipes |> ignore
 *   else
 *     let pid' = Unix.fork () in
 *     if pid' = 0 then
 *       run t b recipes |> ignore
 *     else
 *       let () =
 *         ignore @@ (Unix.waitpid [] pid) in
 *       ignore (Unix.waitpid [] pid')
 *
 * let run_artifacts t tasks =
 *   match tasks with
 *   | [ [a;b],recipes ] -> run_parallel t a b recipes; t
 *   | _ -> run_artifacts t tasks *)
