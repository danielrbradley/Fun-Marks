module Assessments.Core

open System

open Registers.Core

type AssessmentId =
| AssessmentId of Guid

type RegisterSource = 
| Private
| Shared of RegisterId

type Registration =
| Absent
| Present
| Withdrawn

type Mark =
| Mark of decimal
