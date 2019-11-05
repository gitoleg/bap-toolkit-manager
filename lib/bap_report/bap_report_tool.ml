open Core_kernel
open Bap_report_types
open Bap_report_utils

module Recipe = Bap_report_recipe

type recipe = Bap_report_recipe.t


type kind =
  | Image of image
  | Host

type t = {
  kind : kind;
  recipes : recipe list;
  bap_ver : string;
}

let find_bap_version = function
  | Image im -> Image.run im "--version"
  | Host -> cmd "bap --version"

let split_on_first s ~on =
  let indexes = List.filter_map on ~f:(String.index s) in
  let indexes = List.sort ~compare:Int.compare indexes in
  match indexes with
  | [] -> [s]
  | i :: _ ->
    [String.subo ~len:i s;
     String.subo ~pos:(i + 1) s]

let recipes_of_string s =
  let recipe_of_string s =
    match split_on_first ~on:[' '; '\t'] s with
    | name :: desc :: _ ->
      let name = String.strip name in
      let desc = String.strip desc in
      Some (Recipe.create ~name ~desc)
    | _ -> None in
  let rs = String.split ~on:'\n' s in
  List.filter_map rs ~f:recipe_of_string

let collect_recipes kind =
  let str = match kind with
    | Host -> cmd "bap --list-recipes"
    | Image im -> Image.run im "--list-recipes" in
  match str with
  | Error _ | Ok "" -> []
  | Ok s -> recipes_of_string s

let create kind =
  match find_bap_version kind with
  | Error _ as er -> er
  | Ok bap_ver ->
    let recipes = collect_recipes kind in
    Ok {kind;recipes;bap_ver}

let of_image im = create (Image im)
let host () = create Host

let recipes t = t.recipes

let bap_version t = t.bap_ver

let find_recipe t recipe =
  List.find t.recipes
    ~f:(fun r -> String.equal (Recipe.name r) recipe)

let image t = match t.kind with
  | Image im -> Some im
  | Host -> None

let to_string t = match t.kind with
  | Host -> "host"
  | Image im -> Image.to_string im
