module Assessments.State

open Core
open Registers.Core
open Registers.State

module CandidateState =
    type CandidateState = {
            Identity : CandidateId;
            Registration : Option<Registration>;
            Mark : Option<Mark>
        }

    let Default identity =
        { Identity = identity;
          Registration = None;
          Mark = None; }

open CandidateState

type AssessmentState (identity, privateRegisterId, registerSource, candidates) =
    member me.Identity : AssessmentId = identity
    member me.RegisterSource : RegisterSource = registerSource
    member me.PrivateRegisterId : RegisterId = privateRegisterId
    member me.Candidates : List<CandidateState> = candidates
    member me.RegisterIdentity =
        match me.RegisterSource with
        | Private -> me.PrivateRegisterId
        | Shared(registerId) -> registerId

    new(identity, privateRegisterId) =
        AssessmentState(
            identity,
            privateRegisterId, 
            Private,
            List.empty)

let Create identity privateRegisterId =
    new AssessmentState(identity, privateRegisterId)

let Restore identity privateRegisterId registerSource candidates =
    new AssessmentState(identity, privateRegisterId, registerSource, candidates)

let setRegisterSource source candidates (assessment:AssessmentState) =
    let candidateStates =
        candidates
        |> List.map (fun candidateId -> CandidateState.Default candidateId)
    AssessmentState(assessment.Identity, assessment.PrivateRegisterId, source, candidateStates)

exception CandidateNotFoundException of string
exception DuplicateCandidateException of string

let private withCandidates candidates (assessment:AssessmentState) =
    AssessmentState(assessment.Identity, assessment.PrivateRegisterId, assessment.RegisterSource, candidates)

let private assertCandidateExists candidateId (assessment:AssessmentState) =
    if not(assessment.Candidates |> List.exists (fun c -> c.Identity = candidateId)) 
    then raise (CandidateNotFoundException("Candidate to remove not in assessment."))

let private assertCandidateNotAdded candidateId (assessment:AssessmentState) =
    if assessment.Candidates |> List.exists (fun c -> c.Identity = candidateId) 
    then raise (DuplicateCandidateException("Candidate to add already in assessment."))

let private updateCandidate candidateId candidateChange (assessment:AssessmentState) =
    assertCandidateExists candidateId assessment
    let candidates = 
        assessment.Candidates
        |> List.map (fun c ->
            match c with
            | _ when c.Identity = candidateId -> candidateChange c
            | _ -> c)
    assessment |> withCandidates candidates

let addCandidate candidateId (assessment:AssessmentState) =
    assessment |> assertCandidateNotAdded candidateId
    assessment |> withCandidates (assessment.Candidates @ [Default candidateId])

let removeCandidate candidateId (assessment:AssessmentState) =
    assessment |> assertCandidateExists candidateId
    assessment |> withCandidates (
        assessment.Candidates
        |> List.filter (fun c -> not(c.Identity = candidateId)))

let setMark candidateId mark (assessment:AssessmentState) =
    assessment |> assertCandidateExists candidateId
    assessment |> (updateCandidate candidateId (fun c -> { c with Mark = mark }))

let setRegistration candidateId registration (assessment:AssessmentState) =
    assessment |> assertCandidateExists candidateId
    assessment |> (updateCandidate candidateId (fun c -> { c with Registration = registration }))

let rec private insert v i l =
    match i, l with
    | 0, xs -> v::xs
    | i, x::xs -> x::insert v (i - 1) xs
    | i, [] -> failwith "index out of range"

let rec private remove i l =
    match i, l with
    | 0, x::xs -> xs
    | i, x::xs -> x::remove (i - 1) xs
    | i, [] -> failwith "index out of range"

let private move i i' l =
    let v = List.nth l i
    let l' = remove i l
    insert v i' l'

let setCandidateIndex candidateId index (assessment:AssessmentState) =
    let currentIndex = assessment.Candidates |> List.findIndex (fun c -> c.Identity = candidateId)
    assessment |> withCandidates (assessment.Candidates |> move currentIndex index)