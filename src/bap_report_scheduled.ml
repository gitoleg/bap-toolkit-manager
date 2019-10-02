open Core_kernel
open Bap_report.Std

type scheduled = {
  artifact : string;
  recipes  : recipe list
}


module Parse = struct

  let report_error_and_exit arti reason =
    eprintf "can't parse schedule for %s: %s\n" arti
      (Error.to_string_hum reason);
    exit 1

  let malformed_param s =
    Or_error.errorf
      "malformed parameter %s,
       need to be in the form (param value) OR param=value\n"
      (Sexp.to_string s)

  let parse_param s =
    match s with
    | Sexp.(List [Atom name; Atom value]) -> Ok (name,value)
    | _ -> malformed_param s

  let parse_params s =
    let open Sexp in
    match s with
    | List [Atom a; Atom b] -> Ok (a,b)
    | List _ -> malformed_param s
    | Atom a ->
      match String.split a ~on:'=' with
      | [a;b] -> Ok (a,b)
      | _ -> malformed_param s

  let all ~f xs = Result.all (List.map ~f xs)
  let (>>=) = Result.(>>=)

  let make_recipe tool name params =
    match Recipe.find tool name with
    | None -> Or_error.errorf "can't find recipe %s\n" name
    | Some r ->
       Ok (List.fold ~init:r params ~f:(fun r (name,value) ->
           Recipe.add_parameter r ~name ~value))

  let parse_recipe tool s =
    let open Sexp in
    match s with
    | Atom name -> make_recipe tool name []
    | List (Atom name :: params) ->
       all ~f:parse_params params >>= fun params ->
       make_recipe tool name params
    | _ -> Or_error.errorf "can't parse schedule line %s\n" (Sexp.to_string s)

  let of_file tool path : scheduled list =
    let rec read acc = function
      | [] -> List.rev acc
      | line :: lines  ->
        let line = sprintf "(%s)" line in
        match Sexp.of_string line with
        | Sexp.List (Sexp.Atom artifact :: recipes) ->
           let acc = match all ~f:(parse_recipe tool) recipes with
            | Ok recipes -> {artifact; recipes } :: acc
            | Error er -> report_error_and_exit artifact er  in
          read acc lines
        | _ -> read acc lines in
    read [] (In_channel.with_file path ~f:(Read.Helper.lines ~comments:"#"))

end

let of_file = Parse.of_file
