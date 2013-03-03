module Assessments.EventStore

open Core
open Registers.Core

type AssessmentEvent =
| RegisterSourceSet of RegisterId
| CandidateAdded of CandidateId
| CandidateRemoved of CandidateId
| CandidateMarkSet of CandidateId * Mark
| CandidateRegistrationSet of CandidateId * Registration
