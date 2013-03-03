module Registers.State

open System
open Core

type Candidate = {
        Identity : CandidateId;
        Name : string
    }

type RegisterState (identity, candidates) =
    member me.Identity : RegisterId = identity
    member me.Candidates : Map<CandidateId, Candidate> = candidates
    new(identity) = RegisterState(identity, Map.empty)

exception CandidateNotFoundException of string
exception DuplicateCandidateException of string

let private withCandidates candidates (register:RegisterState) =
    RegisterState(register.Identity, candidates)

let private assertCandidateExists candidateId (register:RegisterState) =
    if not(register.Candidates |> Map.containsKey candidateId)
    then raise (CandidateNotFoundException("Candidate to remove not in register."))

let private assertCandidateNotExists candidateId (register:RegisterState) =
    if register.Candidates |> Map.containsKey candidateId
    then raise (DuplicateCandidateException("Candidate to add already in register."))

let private updateCandidate candidateId candidateChange (register:RegisterState) =
    assertCandidateExists candidateId register
    let candidates = 
        register.Candidates
        |> Map.map (fun id c ->
            match id with
            | _ when id = candidateId -> candidateChange c
            | _ -> c)
    register |> withCandidates candidates

let addCandidate candidate (register:RegisterState) =
    register |> assertCandidateNotExists candidate.Identity
    register |> withCandidates (register.Candidates |> Map.add candidate.Identity candidate)

let removeCandidate candidateId (register:RegisterState) =
    register |> assertCandidateExists candidateId
    let candidates = register.Candidates |> Map.remove candidateId
    register |> withCandidates candidates

let updateName candidateId name (register:RegisterState) =
    register |> assertCandidateExists candidateId
    register |> updateCandidate candidateId (fun c -> { c with Name = name })

let hasCandidate candidateId (register:RegisterState) =
    register.Candidates |> Map.containsKey candidateId
