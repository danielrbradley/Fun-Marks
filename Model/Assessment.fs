module Model.Assessment

open System

type Registration =
| Absent
| Present
| Withdrawn

type Mark =
| Mark of decimal

module Candidate =
    type Candidate = {
            Identity : Register.CandidateId;
            Registration : Option<Registration>;
            Mark : Option<Mark>
        }

    let Default identity =
        { Identity = identity;
          Registration = None;
          Mark = None; }

open Candidate

type AssessmentId =
| AssessmentId of Guid

type RegisterSource = 
| RegisterSource of Register.RegisterId

type Assessment (identity, registerSource, candidates) =
    member me.Identity : AssessmentId = identity
    member me.RegisterSource : RegisterSource = registerSource
    member me.Candidates : List<Candidate> = candidates

    new(identity, registerSource) =
        Assessment(
            identity, 
            registerSource, 
            List.empty)

let Create identity registerSource =
    new Assessment(identity, registerSource)

let Restore identity registerSource candidates =
    new Assessment(identity, registerSource, candidates)

let setRegisterSource registerId (assessment:Assessment) =
    Assessment(assessment.Identity, RegisterSource(registerId), assessment.Candidates)

exception CandidateNotFoundException of string
exception DuplicateCandidateException of string

let private withCandidates candidates (assessment:Assessment) =
    Assessment(assessment.Identity, assessment.RegisterSource, candidates)

let private assertCandidateExists candidateId (assessment:Assessment) =
    if not(assessment.Candidates |> List.exists (fun c -> c.Identity = candidateId)) 
    then raise (CandidateNotFoundException("Candidate to remove not in assessment."))

let private assertCandidateNotAdded candidateId (assessment:Assessment) =
    if assessment.Candidates |> List.exists (fun c -> c.Identity = candidateId) 
    then raise (DuplicateCandidateException("Candidate to add already in assessment."))

let private updateCandidate candidateId candidateChange (assessment:Assessment) =
    assertCandidateExists candidateId assessment
    let candidates = 
        assessment.Candidates
        |> List.map (fun c ->
            match c with
            | _ when c.Identity = candidateId -> candidateChange c
            | _ -> c)
    assessment |> withCandidates candidates

let addCandidate candidateId (assessment:Assessment) =
    assessment |> assertCandidateNotAdded candidateId
    assessment |> withCandidates (assessment.Candidates @ [Candidate.Default candidateId])

let removeCandidate candidateId (assessment:Assessment) =
    assessment |> assertCandidateExists candidateId
    assessment |> withCandidates (
        assessment.Candidates
        |> List.filter (fun c -> not(c.Identity = candidateId)))

let setMark candidateId mark (assessment:Assessment) =
    assessment |> assertCandidateExists candidateId
    assessment |> (updateCandidate candidateId (fun c -> { c with Mark = mark }))

let setRegistration candidateId registration (assessment:Assessment) =
    assessment |> assertCandidateExists candidateId
    assessment |> (updateCandidate candidateId (fun c -> { c with Registration = registration }))

let setCandidateIndex candidateId index (assessment:Assessment) =
    let currentIndex = assessment.Candidates |> List.findIndex (fun c -> c.Identity = candidateId)
    assessment |> withCandidates (assessment.Candidates |> List.move currentIndex index)