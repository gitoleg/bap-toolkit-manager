open Core_kernel
open Bap_report_common

module Kind = struct
  type t = string [@@deriving bin_io, compare, sexp]
  let of_string x = x
  let to_string x = x

  module Map = String.Map
  module Set = String.Set
end

module Locations = struct
  type t = addr list [@@deriving bin_io, compare, sexp]

  let create ?(prev=[]) addr = addr :: prev
  let addrs x = x
  let addr xs = List.hd_exn xs

  module T = struct
    type nonrec t = t [@@deriving sexp,compare]
  end

  module S = struct
    include T
    include Comparator.Make(T)
  end

  module Map = Map.Make(S)
  module Set = Set.Make(S)
end

type kind = Kind.t [@@deriving bin_io, compare, sexp]
type locations = Locations.t [@@deriving bin_io, compare, sexp]


module Id = struct

  type t = {
      locs : locations;
      kind : kind;
    } [@@deriving bin_io, compare, sexp]

  let create kind locs = {kind;locs}
  let locations t = t.locs
  let kind t = t.kind

  module T = struct
    type nonrec t = t [@@deriving sexp,compare]
  end

  module S = struct
    include T
    include Comparator.Make(T)
  end

  module Map = Map.Make(S)
  module Set = Set.Make(S)

end

type id = Id.t [@@deriving bin_io, compare, sexp]

type t = {
    id   : id;
    path : string list;
} [@@deriving bin_io, sexp]


let create ?(path=[]) locs kind =
  {id = Id.create kind locs; path}

let locations t = t.id.locs
let path t = t.path
let kind t = t.id.kind
let addr t = Locations.addr t.id.locs

let id t = t.id

let of_id id = create (Id.locations id) (Id.kind id)


let compare t t' =
  let r = Kind.compare t.id.kind t'.id.kind in
  if r <> 0 then r
  else Locations.compare t.id.locs t'.id.locs

module Map = Map.Make(struct
    type nonrec t = t [@@deriving sexp]
    let compare = compare
  end)


module Set = Set.Make(struct
    type nonrec t = t [@@deriving sexp]
    let compare = compare
  end)
