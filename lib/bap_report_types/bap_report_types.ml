
include Bap_report_common

module Incident = Bap_report_incident
module Artifact = Bap_report_artifact
module Confirmation = Bap_report_confirmation

type artifact = Artifact.t
type incident = Incident.t [@@deriving bin_io,compare,sexp]
type incident_kind = Incident.kind [@@deriving bin_io,compare,sexp]
type confirmation = Confirmation.t [@@deriving bin_io,compare,sexp]
type confirmation_kind = Confirmation.kind [@@deriving bin_io,compare,sexp]
