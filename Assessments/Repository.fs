module Assessments.Repository

open Core
open Registers.Core

type AssessmentCommand =
| Get
| SetRegisterSource of RegisterSource * List<CandidateId>
| AddCandidate of CandidateId
| RemoveCandidate of CandidateId
| SetCandidateMark of CandidateId * Mark
| SetCandidateRegistration of CandidateId * Registration

type AssessmentAggregateRoot (commandHandler) =
    member me.State = Get |> commandHandler
    member me.SetRegisterSource source candidates = commandHandler <| SetRegisterSource(source, candidates)
    member me.AddCandidate candidateId = commandHandler <| AddCandidate(candidateId)
    member me.RemoveCandidate candidateId = commandHandler <| RemoveCandidate(candidateId)
    member me.SetCandidateMark candidateId mark = commandHandler <| SetCandidateMark(candidateId, mark)
    member me.SetCandidateRegistration candidateId registration = commandHandler <| SetCandidateRegistration(candidateId, registration)

exception AssessmentAlreadyCreatedException of System.Guid * string
exception AssessmentNotFoundException of System.Guid * string

let private failWithAlreadyCreated identity message =
    match identity with
    | AssessmentId(guid) ->
        raise (AssessmentAlreadyCreatedException (guid, message))

let private failWithNotFound identity message =
    match identity with
    | AssessmentId(guid) ->
        raise (AssessmentNotFoundException (guid, message))

type AssessmentRepository (commandHandlerFactory) =
    let commandHandlers = new System.Collections.Concurrent.ConcurrentDictionary<AssessmentId, AssessmentCommand -> State.AssessmentState>()
    member me.Create identity (registerId:RegisterId) =
        let commandHandler = commandHandlerFactory identity registerId
        let assessment = new AssessmentAggregateRoot(commandHandler)
        if not(commandHandlers.TryAdd(identity, commandHandler)) then failWithAlreadyCreated identity "Assessment already created"
        assessment

    member me.Open identity =
        let found, commandHandler = commandHandlers.TryGetValue(identity)
        if not(found) then failWithNotFound identity "Assessment not found"
        new AssessmentAggregateRoot(commandHandler)
