open Core_kernel
open Bap_report_common

(* there are only two kinds of confirmed incidents:
    - must - such one which MUST come up during the analysis
      and therefore an absence of such incindent is False negative,
      and a presence is a Confirmed incident

    - may - such one which MAY come up during the analysis.
      and therefore an absence of such incindent is not a mistake,
      and a presence is False positive.  *)


type kind =
  | Must
  | May
[@@deriving sexp,compare,bin_io]

type t = {
    locs : addr list;
    kind : Bap_report_incident.kind;
    conf : kind;
  } [@@deriving sexp,compare,bin_io]


let create conf kind locs = {locs; kind; conf}
let locations t = t.locs
let kind t = t.kind
let confirmation t = t.conf


let validate {conf} status =
  match conf,status with
  | May, None -> Undecided
  | May, Some _ -> False_pos
  | Must, None -> False_neg
  | Must, Some _ -> Confirmed
