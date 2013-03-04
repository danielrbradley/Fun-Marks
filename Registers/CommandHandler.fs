module Registers.CommandHandler

open Core
open State
open Repository

type RegisterEvent =
| CandidateAdded of CandidateId * string
| CandidateRemoved of CandidateId
| CandidateNameSet of CandidateId * string

let apply event state =
    match event with
    | CandidateAdded(candidateId, name) -> state |> addCandidate { Identity = candidateId; Name = name; }
    | CandidateRemoved(candidateId) -> state |> removeCandidate candidateId
    | CandidateNameSet(candidateId, name) -> state |> updateName candidateId name

let replay registerId events =
    let initialState = new RegisterState(registerId)
    let folder = fun state event -> state |> apply event
    events |> Seq.fold folder initialState

let createEvent command =
    match command with
    | Get -> None
    | AddCandidate(candidateId, name) -> Some(CandidateAdded(candidateId, name))
    | RemoveCandidate(candidateId) -> Some(CandidateRemoved(candidateId))
    | SetCandidateName(candidateId, name) -> Some(CandidateNameSet(candidateId, name))

let createCommandHandler registerId =
    let initialState = new RegisterState(registerId)
    let writeEvent event = ()
    let commandHandler = EventStoreAgent.create initialState createEvent apply writeEvent
    commandHandler.Execute
