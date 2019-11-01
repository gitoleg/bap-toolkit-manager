open Core_kernel
open Bap_report.Std
open Bap_report_options

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

  let create ?confirmations ?store ~output ctxt =
    let t = {
      ctxt;
      tasks = Map.empty (module String);
      output;
      confirmed = read_confirmations confirmations;
      expected = Map.empty (module Journal);
      store = None;
    } in
    match store with
    | Some (file,update) ->
      let t =
        if update then
          let artis = Bap_report_io.read file in
          List.fold artis ~init:t ~f:(fun t a -> add_task t a)
        else t in
      {t with store = Some file}
    | None -> t

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

  let render ?ready t =
    let doc = Template.render (artifacts ?names:ready t) in
    Out_channel.with_file t.output
      ~f:(fun ch -> Out_channel.output_string ch doc)

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

  let notify_started j =
    printf "%s: started %s\n%!" (startup_time ()) (Job.name j)

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
        let j = Job.run t.ctxt j in
        let journal = Job.journal j in
        match find_by_journal t journal with
        | None -> t,ready
        | Some task ->
          let t = update_task t task journal in
          let ready = Set.add ready (name_of_task task) in
          render t ~ready;
          t, ready) |> fst

  let run_threads ctxt ts =
    let run jobs =
      List.iter jobs ~f:(fun j ->
          ignore @@ Job.run ctxt j) in
    let rec loop acc = function
      | [] -> acc
      | t :: ts ->
        List.iter t ~f:notify_started;
        match Unix.fork () with
        | 0 ->
          List.iter ~f:Unix.close Unix.[stdout; stderr; stdin];
          run t;
          exit 0
        | pid -> loop (pid :: acc) ts in
    let waits = loop [] ts in
    List.iter waits ~f:(fun pid ->
        ignore @@ Unix.waitpid [] pid)

  let plain_balance xs n =
    let procs = Array.init n ~f:(fun _ -> []) in
    let _ =
      List.fold xs ~init:0
        ~f:(fun i x ->
            Array.set procs i (x :: procs.(i));
            (i + 1) mod n) in
    Array.to_list procs |> List.filter ~f:(fun x -> x <> [])

  let run_parallel t jobs n =
    let threads = plain_balance jobs n in
    run_threads t.ctxt threads;
    let t = List.fold jobs ~init:t ~f:(fun t j ->
        let journal = Job.journal j in
        match find_by_journal t journal with
        | None -> t
        | Some task -> update_task t task journal) in
    render t;
    t

  let save t =
    match t.store with
    | None -> ()
    | Some file -> Bap_report_io.dump file (artifacts t)

  let run t xs j =
    let t,jobs = prepare_jobs t xs in
    match j with
    | n when n < 2 -> run_seq t jobs |> save
    | n -> run_parallel t jobs n |> save

  let of_incidents_file t filename =
    let name = Filename.remove_extension filename in
    let incidents = In_channel.with_file filename ~f:Read.incidents in
    let artifact = Artifact.create name in
    let artifact = List.fold incidents ~init:artifact
        ~f:(fun a i -> Artifact.update a i Undecided) in
    let artifact = confirm t.confirmed artifact (Artifact.checks artifact) in
    let t = add_task t artifact in
    render t

  let of_db t db =
    let artis = Bap_report_io.read db in
    let t = List.fold artis ~init:t ~f:add_task in
    render t
end

let main o =
  let store = Option.map o.store ~f:(fun x -> x, o.update) in
  let t = Run.create ?confirmations:o.confirms ?store ~output:o.output o.context in
  match o.mode with
  | From_incidents incs -> Run.of_incidents_file t incs
  | From_stored db -> Run.of_db t db
  | Run_artifacts tasks -> Run.run t tasks o.jobs

let _ =
  let open Cmdliner in
  let open Bap_report_cmd_terms in
  Term.eval (Term.(const main $options), info)

(* TODO: install view file somewhere
   TODO: maybe rewise all the directories/archives creation:
         maybe make more distiguive names or place everything in the
         temp dir
   TODO: check limits once more again
   TODO: set limits back ? in the case of host
   TODO: try to launch on a fresh system. Is true that we'll pull the
         images ?
   TODO: also, journals and storing to db looks a little bit redundant

*)
