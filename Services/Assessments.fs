namespace Services

open System

open Assessments.Core
open Assessments.Repository
open Registers.Core
open Registers.Repository

type Assessments (assessmentRepo:AssessmentRepository, registerRepo:RegisterRepository) =
    member me.Create =
        let registerId = RegisterId(Guid.NewGuid())
        let repository = registerRepo.Create(registerId)
        let assessmentId = AssessmentId(Guid.NewGuid())
        assessmentRepo.Create assessmentId registerId
        assessmentId

    member me.Get assessmentId =
        let identity = AssessmentId(assessmentId)
        assessmentRepo.Open(identity).State

    member me.SetSharedRegisterSource assessmentId registerId =
        let identity = AssessmentId(assessmentId)
        let assessmentAR = assessmentRepo.Open(identity)
        let registerIdentity = RegisterId(registerId)
        let register = registerRepo.Open(registerIdentity)
        let candidates = 
            register.State.Candidates
            |> List.sortBy (fun candidate -> candidate.Name)
            |> List.map (fun candidate -> candidate.Identity)
        assessmentAR.SetRegisterSource (Shared(registerIdentity)) candidates

    member me.AddCandidate assessmentId candidateId =
        let identity = AssessmentId(assessmentId)
        let candidateIdentity = CandidateId(candidateId)
        assessmentRepo.Open(identity).AddCandidate candidateIdentity

    member me.RemoveCandidate assessmentId candidateId =
        let identity = AssessmentId(assessmentId)
        let candidateIdentity = CandidateId(candidateId)
        assessmentRepo.Open(identity).RemoveCandidate candidateIdentity

    member me.SetCandidateMark assessmentId candidateId mark =
        let identity = AssessmentId(assessmentId)
        let candidateIdentity = CandidateId(candidateId)
        assessmentRepo.Open(identity).SetCandidateMark candidateIdentity mark

    member me.SetCandidateRegistration assessmentId candidateId registration =
        let identity = AssessmentId(assessmentId)
        let candidateIdentity = CandidateId(candidateId)
        assessmentRepo.Open(identity).SetCandidateRegistration candidateIdentity registration
