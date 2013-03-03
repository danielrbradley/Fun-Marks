module Repository.Assessments

open Model.Register
open Model.Assessment

type AssessmentEvent =
| RegisterSourceSet of RegisterId
| CandidateAdded of CandidateId
| CandidateRemoved of CandidateId
| CandidateMarkSet of CandidateId * Mark
| CandidateRegistrationSet of CandidateId * Registration

type AssessmentCommand =
| Get
| SetRegisterSource of RegisterId
| AddCandidate of CandidateId
| RemoveCandidate of CandidateId
| SetCandidateMark of CandidateId * Mark
| SetCandidateRegistration of CandidateId * Registration

type Assessment (commandHandler) =
    member me.State = Get |> commandHandler 

    member me.SetRegisterSource registerId = commandHandler <| SetRegisterSource(registerId)
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

type AssessmentRepo (commandHandlerFactory) =
    let commandHandlers = new System.Collections.Concurrent.ConcurrentDictionary<AssessmentId, AssessmentCommand -> Model.Assessment.Assessment>()
    member me.Create (identity) registerSource =
        let commandHandler = commandHandlerFactory identity
        let assessment = new Assessment(commandHandler)
        if not(commandHandlers.TryAdd(identity, commandHandler)) then failWithAlreadyCreated identity "Assessment already created"

    member me.Open identity =
        let found, commandHandler = commandHandlers.TryGetValue(identity)
        if not(found) then failWithNotFound identity "Assessment not found"
        new Assessment(commandHandler)
