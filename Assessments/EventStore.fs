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
| CandidateMarkSet of CandidateId * Option<Mark>
| CandidateRegistrationSet of CandidateId * Option<Registration>

type Query =
| Query of AssessmentCommand * AsyncReplyChannel<AssessmentState>

let apply event state =
    match event with
    | RegisterSourceSet(source, candidates) -> state |> setRegisterSource source candidates
    | NameSet(name) -> state |> setName name
    | CandidateAdded(candidateId) -> state |> addCandidate candidateId
    | CandidateRemoved(candidateId) -> state |> removeCandidate candidateId
    | CandidateMarkSet(candidateId, mark) -> state |> setMark candidateId mark
    | CandidateRegistrationSet(candidateId, registration) -> state |> setRegistration candidateId registration

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
    | SetCandidateMark(candidateId, mark) -> Some(CandidateMarkSet(candidateId, mark))
    | SetCandidateRegistration(candidateId, registration) -> Some(CandidateRegistrationSet(candidateId, registration))

let eventStoreCommandHandler initialState writeEvent =
    MailboxProcessor.Start <| fun inbox ->
        let rec Loop state = 
            async {
                let! query = inbox.Receive()
                match query with
                | Query(command, replyChannel) ->
                    let eventOption = createEvent command
                    match eventOption with
                    | Some(event) ->
                        let newState = state |> apply event
                        writeEvent event
                        replyChannel.Reply newState
                        do! Loop newState
                    | None ->
                        replyChannel.Reply state
                        do! Loop state
            }
        Loop initialState