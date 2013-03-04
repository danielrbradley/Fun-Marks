module Assessments.Core

open System

open Registers.Core

type AssessmentId =
| AssessmentId of Guid

type RegisterSource = 
| Private
| Shared of RegisterId

type Result =
| Mark of decimal
| Absent of string
| Withdrawn of string
