open Core_kernel
open Bap_report_common

type kind = string [@@deriving bin_io, compare, sexp]

type locs = addr list [@@deriving bin_io, compare, sexp]

type t = {
    kind : kind;
    addr : addr;
    locs : locs;
    path : string list;
} [@@deriving bin_io, sexp]


let create ?(path=[]) ?(locs=[]) kind addr =
  {kind; addr; locs; path}

let locations t = t.locs
let path t = t.path
let kind t = t.kind
let addr t = t.addr

let kind_of_string x = x
let string_of_kind x = x

let compare t t' =
  let r = compare_kind t.kind t'.kind in
  if r <> 0 then r
  else compare_locs t.locs t'.locs

module Map = Map.Make(struct
    type nonrec t = t [@@deriving sexp]
    let compare = compare
  end)


module Set = Set.Make(struct
    type nonrec t = t [@@deriving sexp]
    let compare = compare
  end)
