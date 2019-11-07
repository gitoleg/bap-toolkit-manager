open Core_kernel
open Bap_report.Std
open Bap_report_options

module IO = Bap_report_io
module Progress = Bap_report_progress

module Run = struct

  type task = {
    artifact : artifact;
    journals : journal list
  }

  type t = {
    ctxt      : Job.ctxt;
    tasks     : task String.Map.t;
    output    : string;
    store     : string option;
    expected  : Incident.Kind.Set.t Journal.Map.t;
    confirmed : confirmation Incident.Id.Map.t String.Map.t;
  }

  type worker = {
      pid  : int;
      wait : bool;
      chan : In_channel.t;
  }

  let new_task artifact = {
    artifact;
    journals = []
  }

  let add_task t arti =
    let task = new_task arti in
    {t with tasks = Map.set t.tasks (Artifact.name arti) task}

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

  let read_db t file =
    match IO.Artifacts.read file with
    | Error er ->
       eprintf "can't read file %s: %s\n"
         file (Error.to_string_hum er);
       t
    | Ok artis ->
       List.fold artis ~init:t ~f:(fun t a -> add_task t a)

  let create ?confirmations ?store ~output ctxt = {
      ctxt;
      tasks = Map.empty (module String);
      output;
      confirmed = read_confirmations confirmations;
      expected = Map.empty (module Journal);
      store;
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
        if mem name then data.artifact :: artis
        else artis)

  let render_artifacts output xs =
    let doc = Template.render xs in
    Out_channel.with_file output
      ~f:(fun ch -> Out_channel.output_string ch doc)

  let render ?ready t =
    let artis = artifacts ?names:ready t in
    render_artifacts t.output artis

  let find_by_journal t j =
    Map.data t.tasks |>
    List.find ~f:(fun task ->
        List.exists task.journals ~f:(fun j' -> Journal.equal j j'))

  let print_errors j =
    List.iter (Journal.errors j) ~f:(eprintf "%s\n")

  let missed_kinds expected incidents =
    let happened = List.fold incidents ~init:(Set.empty (module Incident.Kind))
        ~f:(fun kinds inc -> Set.add kinds (Incident.kind inc)) in
    Set.diff expected happened |> Set.to_list

  let find_expected t journal =
    match Map.find t.expected journal with
    | Some x -> x
    | None -> Set.empty (module Incident.Kind)

  let check_diff xs ys =
    let of_list = Set.of_list (module Incident.Kind) in
    Set.to_list @@ Set.diff (of_list xs) (of_list ys)

  let update_time arti checks = function
    | None -> arti
    | Some time ->
      List.fold checks ~init:arti
        ~f:(fun arti c -> Artifact.with_time arti c time)

  let startup_time () =
    let open Unix in
    let t = gettimeofday () |> localtime in
    sprintf "%02d:%02d:%02d" t.tm_hour t.tm_min t.tm_sec

  let notify action job =
    printf "%s %s %s\n%!" (startup_time ()) action (Job.name job)

  let notify_started job = notify "started" job
  let notify_finished job = notify "finished" job

  let name_of_task t = Artifact.name t.artifact

  let update_task t task journal =
    print_errors journal;
    let incs = Journal.incidents journal in
    let time = Journal.time journal in
    let expected = find_expected t journal in
    let missed = missed_kinds expected incs in
    let checks = Artifact.checks task.artifact in
    let arti = List.fold missed ~init:task.artifact
        ~f:(fun arti kind ->
            let a = Artifact.no_incidents arti kind in
            match time with
            | None -> a
            | Some time -> Artifact.with_time a kind time) in
    let arti = List.fold incs ~init:arti ~f:(fun a i -> Artifact.update a i Undecided) in
    let diff = check_diff (Artifact.checks arti) checks in
    let arti = update_time arti diff time in
    let arti = confirm t.confirmed arti diff in
    let task = { task with artifact=arti } in
    {t with tasks = Map.set t.tasks (Artifact.name arti) task}

  let run_seq t jobs =
    let ready = Set.empty (module String) in
    List.fold jobs ~init:(t,ready) ~f:(fun (t,ready) j ->
        notify_started j;
        match Job.run t.ctxt j with
        | Error er ->
           eprintf "Job %s failed: %s \n"
             (Job.name j) (Error.to_string_hum er);
           t,ready
        | Ok j ->
           let journal = Job.journal j in
           match find_by_journal t journal with
           | None -> t,ready
           | Some task ->
              let t = update_task t task journal in
              let ready = Set.add ready (name_of_task task) in
              render t ~ready;
          t, ready) |> fst

  let try_read ch =
    try IO.Msg.read ch
    with _ -> None

  let mk_worker ?(wait=true) f =
    let (read,write) = Unix.pipe () in
    Unix.set_nonblock read;
    match Unix.fork () with
    | 0 ->
       Unix.close read;
       List.iter ~f:Unix.close Unix.[stdout; stderr; stdin];
       f write;
       Unix.close write;
       exit 0
    | pid ->
       Unix.close write;
       {chan=Unix.in_channel_of_descr read; pid; wait}

  let ticks fd =
    let ch = Unix.out_channel_of_descr fd in
    let rec loop () =
      Unix.sleep 1;
      IO.Msg.(write ch `Tick);
      loop () in
    loop ()

  let incidents jobs fd =
    let ch = Unix.out_channel_of_descr fd in
    let incs = String.Table.create () in
    List.iter jobs ~f:(fun j ->
        Hashtbl.set incs (Job.name j) (0,0L));
    let rec loop () =
      Unix.sleep 10;
      List.iter jobs ~f:(fun job ->
          let j = Job.journal job in
          let num,bookmark = Hashtbl.find_exn incs (Job.name job) in
          let num',bookmark' = Journal.incidents' ~bookmark j in
          Hashtbl.set incs  (Job.name job) (num + num', bookmark');
          IO.Msg.write ch (`Job_incidents (Job.name job, num + num')));
      loop () in
    loop ()

  let run_jobs ctxt jobs n =
    let run j fd =
      let open IO.Msg in
      let ch = Unix.out_channel_of_descr fd in
      try
        write ch (`Job_started (Job.name j));
        match Job.run ctxt j with
        | Ok _ -> write ch (`Job_finished (Job.name j))
        | Error er ->
           let er = Error.to_string_hum er in
           write ch (`Job_errored (Job.name j, er))
      with exn ->
        let er = Exn.to_string exn in
        write ch (`Job_errored (Job.name j, er)) in
    let has_wait xs = List.exists xs ~f:(fun x -> x.wait) in
    let num_workers xs = List.count xs ~f:(fun x -> x.wait) in
    let can_add_workers running awaiting =
      match awaiting with
      | [] -> false
      | _ -> num_workers running < n in
    let rec loop finished awaiting running =
      match running, awaiting with
      | [],[] -> finished
      | running, [] when not (has_wait running) -> finished
      | running, awaiting when can_add_workers running awaiting ->
         let dn = n - num_workers running in
         let next = List.take awaiting dn in
         let next = List.map next ~f:(fun j -> mk_worker (run j)) in
         loop finished (List.drop awaiting dn) (next @ running)
      | running,awaiting ->
         let finished, running =
           List.fold running ~init:(finished,[])
             ~f:(fun (finished,running) x ->
               match try_read x.chan with
               | None -> finished, x::running
               | Some msg ->
                  Progress.render msg;
                  match msg with
                  | `Job_finished _ | `Job_errored _ ->
                     In_channel.close x.chan;
                     x.pid :: finished, running
                  | _ -> finished, x::running) in
         loop finished awaiting running in
    Progress.enable ();
    let ticks = mk_worker ~wait:false ticks in
    let incs  = mk_worker ~wait:false (incidents jobs) in
    let waits = loop [] jobs [ticks; incs] in
    List.iter waits ~f:(fun pid ->
        ignore @@ Unix.waitpid [] pid);
    Unix.kill ticks.pid Sys.sigkill;
    Unix.kill incs.pid  Sys.sigkill

  let run_parallel t jobs n =
    run_jobs t.ctxt jobs n;
    let t = List.fold jobs ~init:t ~f:(fun t j ->
        let journal = Job.journal j in
        match find_by_journal t journal with
        | None -> t
        | Some task -> update_task t task journal) in
    render t;
    t

  let run_jobs ~preserve_journals t xs j =
    let t,jobs = prepare_jobs t xs in
    let t = match j,jobs with
      | n,_ when n < 2 -> run_seq t jobs
      | _, [] | _, [_] -> run_seq t jobs
      | n,jobs -> run_parallel t jobs n in
    if not preserve_journals then
      List.iter jobs ~f:(fun j -> Journal.remove (Job.journal j));
    t

  let run ?(preserve_journals=false) t xs j =
    let of_artis xs =
      List.fold xs ~init:(Map.empty (module String))
        ~f:(fun m a -> Map.set m (Artifact.name a) a) in
    let t = run_jobs ~preserve_journals t xs j in
    match t.store with
    | None -> ()
    | Some f ->
       let artis =
         if Sys.file_exists f then
           match IO.Artifacts.read f with
           | Error er ->
              eprintf "can't read file %s: %s\n" f (Error.to_string_hum er);
              []
           | Ok artis -> artis
         else [] in
       let old_artis = of_artis artis in
       let new_artis = of_artis (artifacts t) in
       let artis = Map.merge new_artis old_artis ~f:(fun ~key -> function
                       | `Left x | `Right x -> Some x
                       | `Both (x,y) -> Artifact.left_merge x y) in
       let artis = Map.data artis in
       IO.Artifacts.dump f artis;
       render_artifacts t.output artis

  let of_incidents_file t filename =
    let name = Filename.remove_extension filename in
    let incidents = In_channel.with_file filename ~f:Read.incidents in
    let artifact = Artifact.create name in
    let artifact = List.fold incidents ~init:artifact
        ~f:(fun a i -> Artifact.update a i Undecided) in
    let artifact = confirm t.confirmed artifact (Artifact.checks artifact) in
    let t = add_task t artifact in
    render t

  let of_file t file = render @@ read_db t file
end

let main o =
  let t = Run.create ?confirmations:o.confirms ?store:o.store ~output:o.output o.context in
  match o.mode with
  | From_incidents incs -> Run.of_incidents_file t incs
  | From_stored file -> Run.of_file t file
  | Run_artifacts tasks ->
     Run.run ~preserve_journals:o.journaling t tasks o.jobs

let _ =
  let open Cmdliner in
  let open Bap_report_cmd_terms in
  Term.eval (Term.(const main $options), info)

(* TODO: install view file somewhere
   TODO: check limits once more again
   TODO: set limits back ? in the case of host
   TODO: try to launch on a fresh system. Is true that we'll pull the
         images ?
   TODO: maybe add a support for bap.1.x

*)
