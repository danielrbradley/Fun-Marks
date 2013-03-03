module Registers.Repository

open Core
open State

type RegisterCommand =
| Get
| AddCandidate of CandidateId * string
| RemoveCandidate of CandidateId
| SetCandidateName of CandidateId * string

type RegisterAggregateRoot (commandHandler) =
    member me.State = Get |> commandHandler
    member me.AddCandidate candidateId name = commandHandler <| AddCandidate(candidateId, name)
    member me.RemoveCandidate candidateId = commandHandler <| RemoveCandidate(candidateId)
    member me.SetCandidateName candidateId name = commandHandler <| SetCandidateName(candidateId, name)

exception RegisterAlreadyCreatedException of System.Guid * string
exception RegisterNotFoundException of System.Guid * string

let private failWithAlreadyCreated identity message =
    match identity with
    | RegisterId(guid) ->
        raise (RegisterAlreadyCreatedException (guid, message))

let private failWithNotFound identity message =
    match identity with
    | RegisterId(guid) ->
        raise (RegisterNotFoundException (guid, message))

type RegisterRepository (commandHandlerFactory) =
    let commandHandlers = new System.Collections.Concurrent.ConcurrentDictionary<RegisterId, RegisterCommand -> RegisterState>()
    member me.Create (identity) registerSource =
        let commandHandler = commandHandlerFactory identity
        let assessment = new RegisterAggregateRoot(commandHandler)
        if not(commandHandlers.TryAdd(identity, commandHandler)) then failWithAlreadyCreated identity "Assessment already created"

    member me.Open identity =
        let found, commandHandler = commandHandlers.TryGetValue(identity)
        if not(found) then failWithNotFound identity "Assessment not found"
        new RegisterAggregateRoot(commandHandler)
