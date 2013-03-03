namespace Services.Assessments

open System

open Assessments.Core
open Assessments.Repository
open Registers
open Registers.Core
open Registers.Repository

exception CandidateNotFoundException of Guid * string

type Registration =
| NotSet = 0
| Present = 1
| Absent = 2
| Withdrawn = 3

type Candidate = {
        Identity : Guid;
        Name : string;
        Registration : Registration
        Mark : Nullable<decimal>;
    }

type Assessment = {
        Identity : Guid;
        Candidates : List<Candidate>
    }

type Assessments (assessmentRepo:AssessmentRepository, registerRepo:RegisterRepository) =
    let convertFromRegistration (registration:Option<Assessments.Core.Registration>) =
        match registration with
        | Some(value) ->
            match value with
            | Present -> Registration.Present
            | Absent -> Registration.Present
            | Withdrawn -> Registration.Withdrawn
        | None -> Registration.NotSet

    let convertToRegistration (registration:Registration) =
        match registration with
        | Registration.NotSet -> None
        | Registration.Present -> Some(Present)
        | Registration.Absent -> Some(Absent)
        | Registration.Withdrawn -> Some(Withdrawn)
        | _ -> failwith "Invalid registration"

    let convertFromMark (markOption:Option<Assessments.Core.Mark>) =
        match markOption with
        | Some(mark) ->
            match mark with
            | Mark(value) -> new Nullable<decimal>(value)
        | None -> Nullable<decimal>()

    let convertToMark (mark:Nullable<decimal>) =
        match mark.HasValue with
        | true -> Some(Mark(mark.Value))
        | false -> None

    let constructCandidate (assessmentCandidate:Assessments.State.CandidateState.CandidateState) (registerCandidate:Registers.State.Candidate) =
        { Identity = match assessmentCandidate.Identity with CandidateId(guid) -> guid;
          Name = registerCandidate.Name;
          Registration = convertFromRegistration assessmentCandidate.Registration;
          Mark = convertFromMark assessmentCandidate.Mark }

    let constructCandidates (assessmentCandidates:List<Assessments.State.CandidateState.CandidateState>) (registerCandidates:Map<CandidateId,Registers.State.Candidate>) =
        assessmentCandidates
        |> List.map (fun assessmentCandidate ->
                        let registerCandidate =
                            registerCandidates.Item assessmentCandidate.Identity
                        constructCandidate assessmentCandidate registerCandidate)

    let constructAssessment (assessment:Assessments.State.AssessmentState) (register:Registers.State.RegisterState) =
        if not (assessment.RegisterIdentity = register.Identity) then failwith "Incorrect register loaded for assessment"
        else { Identity = match assessment.Identity with AssessmentId(id) -> id
               Candidates = constructCandidates assessment.Candidates register.Candidates }

    let failWithCandidateNotFound candidateId =
        raise (CandidateNotFoundException(candidateId, "Candidate not found in register."))

    member me.Create name =
        let registerId = RegisterId(Guid.NewGuid())
        let repository = registerRepo.Create registerId
        let assessmentId = AssessmentId(Guid.NewGuid())
        let assessment = assessmentRepo.Create assessmentId registerId
        assessment.SetName name |> ignore
        assessmentId

    member me.Get assessmentId =
        let identity = AssessmentId(assessmentId)
        let assessment = assessmentRepo.Open(identity)
        constructAssessment assessment.State

    member me.SetSharedRegisterSource assessmentId registerId =
        let identity = AssessmentId(assessmentId)
        let assessment = assessmentRepo.Open(identity)
        let registerIdentity = RegisterId(registerId)
        let register = registerRepo.Open(registerIdentity)
        let candidates = 
            register.State.Candidates
            |> Map.toList
            |> List.map snd
            |> List.sortBy (fun candidate -> candidate.Name)
            |> List.map (fun candidate -> candidate.Identity)
        assessment.SetRegisterSource (Shared(registerIdentity)) candidates

    member me.SetName assessmentId name =
        let identity = AssessmentId(assessmentId)
        let assessment = assessmentRepo.Open(identity)
        assessment.SetName name |> ignore

    member me.AddCandidate (assessmentId, candidateId) =
        let identity = AssessmentId(assessmentId)
        let candidateIdentity = CandidateId(candidateId)
        let assessment = assessmentRepo.Open(identity)
        let register = registerRepo.Open(assessment.State.RegisterIdentity)
        let hasCandidate = register.State |> State.hasCandidate candidateIdentity
        if not hasCandidate then failWithCandidateNotFound candidateId
        assessment.AddCandidate candidateIdentity |> ignore

    member me.AddCandidate (assessmentId, name) =
        let identity = AssessmentId(assessmentId)
        let assessment = assessmentRepo.Open(identity)
        let register = registerRepo.Open(assessment.State.RegisterIdentity)
        let candidateId = Guid.NewGuid()
        let candidateIdentity = CandidateId(candidateId)
        register.AddCandidate candidateIdentity name |> ignore
        candidateId

    member me.RemoveCandidate assessmentId candidateId =
        let identity = AssessmentId(assessmentId)
        let candidateIdentity = CandidateId(candidateId)
        assessmentRepo.Open(identity).RemoveCandidate candidateIdentity |> ignore

    member me.SetCandidateMark assessmentId candidateId mark =
        let identity = AssessmentId(assessmentId)
        let candidateIdentity = CandidateId(candidateId)
        let parsedMark = mark |> convertToMark
        assessmentRepo.Open(identity).SetCandidateMark candidateIdentity parsedMark |> ignore

    member me.SetCandidateRegistration assessmentId candidateId registration =
        let identity = AssessmentId(assessmentId)
        let candidateIdentity = CandidateId(candidateId)
        let parsedRegistration = registration |> convertToRegistration
        assessmentRepo.Open(identity).SetCandidateRegistration candidateIdentity parsedRegistration |> ignore
