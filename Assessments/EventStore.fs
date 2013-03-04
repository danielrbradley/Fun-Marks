module Assessments.EventStore

open Core
open Registers.Core
open State
open Repository

type AssessmentEvent =
| RegisterSourceSet of RegisterSource * List<CandidateId>
| NameSet of string
| CandidateAdded of CandidateId
| CandidateRemoved of CandidateId
| CandidateResultSet of CandidateId * Option<Result>

let apply event state =
    match event with
    | RegisterSourceSet(source, candidates) -> state |> setRegisterSource source candidates
    | NameSet(name) -> state |> setName name
    | CandidateAdded(candidateId) -> state |> addCandidate candidateId
    | CandidateRemoved(candidateId) -> state |> removeCandidate candidateId
    | CandidateResultSet(candidateId, result) -> state |> setMark candidateId result

let replay assessmentId registerId events =
    let initialState = State.Create assessmentId registerId
    let folder = fun state event -> state |> apply event
    events |> Seq.fold folder initialState

let createEvent command =
    match command with
    | Get -> None
    | SetRegisterSource(registerSource, registerCandidates) -> Some(RegisterSourceSet(registerSource, registerCandidates))
    | SetName(name) -> Some(NameSet(name))
    | AddCandidate(candidateId) -> Some(CandidateAdded(candidateId))
    | RemoveCandidate(candidateId) -> Some(CandidateRemoved(candidateId))
    | SetCandidateResult(candidateId, result) -> Some(CandidateResultSet(candidateId, result))

let createAssessmentCommandHandler assessmentId privateRegisterId =
    let initialState = State.Create assessmentId privateRegisterId
    let writeEvent event = ()
    let commandHandler = new EventStoreAgent.EventStoreComandHandler<AssessmentCommand, AssessmentState>(initialState createEvent apply writeEvent)
    commandHandler.Execute
