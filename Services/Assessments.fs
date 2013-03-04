namespace Services.Assessments

open System

open Assessments.Core
open Assessments.Repository
open Registers
open Registers.Core
open Registers.Repository

exception CandidateNotFoundException of Guid * string

type ResultMissingReason =
| Absent = 0
| Withdrawn = 1

type Candidate = {
        Identity : Guid;
        Name : string;
        Mark : Nullable<decimal>;
        ResultMissingReason : Nullable<ResultMissingReason>;
        ResultMissingReasonComment : string;
    }

type Assessment = {
        Identity : Guid;
        Candidates : List<Candidate>
    }

type IAssessments =
    abstract member Create : string -> Guid
    abstract member Get : Guid -> Assessment
    abstract member SetName : Guid -> string -> unit
    abstract member AddExistingCandidate : Guid -> Guid -> unit
    abstract member AddNewCandidate : Guid -> string -> Guid
    abstract member RemoveCandidate : Guid -> Guid -> unit
    abstract member ClearCandidateMark : Guid -> Guid -> unit
    abstract member SetCandidateMark : Guid -> Guid -> decimal -> unit
    abstract member SetCandidateResultMissingReason : Guid -> Guid -> ResultMissingReason -> string -> unit

type Assessments (assessmentRepo:AssessmentRepository, registerRepo:RegisterRepository) =
    let parseResultMissingReason (resultMissingReason:ResultMissingReason) (resultMissingReasonComment) =
        match resultMissingReason with
        | ResultMissingReason.Absent -> Absent(resultMissingReasonComment)
        | ResultMissingReason.Withdrawn -> Withdrawn(resultMissingReasonComment)
        | _ -> raise (System.ArgumentOutOfRangeException("resultMissingReason", "Invalid reason"))

    let convertFromResult (resultOption:Option<Assessments.Core.Result>) =
        match resultOption with
        | Some(result) ->
            match result with
            | Mark(mark) -> (new Nullable<ResultMissingReason>(), System.String.Empty, new Nullable<decimal>(mark))
            | Absent(comment) -> (new Nullable<ResultMissingReason>(ResultMissingReason.Absent), comment, new Nullable<decimal>())
            | Withdrawn(comment) -> (new Nullable<ResultMissingReason>(ResultMissingReason.Withdrawn), comment, new Nullable<decimal>())
        | None -> (new Nullable<ResultMissingReason>(), System.String.Empty, new Nullable<decimal>())

    let convertToResult (resultMissingReason:Nullable<ResultMissingReason>) (resultMissingReasonComment) (mark:Nullable<decimal>) =
        if mark.HasValue then Some(Mark(mark.Value))
        elif resultMissingReason.HasValue then
            match resultMissingReason.Value with
            | ResultMissingReason.Absent -> Some(Absent(resultMissingReasonComment))
            | ResultMissingReason.Withdrawn -> Some(Withdrawn(resultMissingReasonComment))
            | _ -> failwith ""
        else None

    let constructCandidate (assessmentCandidate:Assessments.State.CandidateState.CandidateState) (registerCandidate:Registers.State.Candidate) =
        let resultMissingReason, resultMissingReasonComment, mark = convertFromResult assessmentCandidate.Result
        { Identity = match assessmentCandidate.Identity with CandidateId(guid) -> guid;
          Name = registerCandidate.Name;
          Mark = mark;
          ResultMissingReason = resultMissingReason;
          ResultMissingReasonComment = resultMissingReasonComment; }

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

    interface IAssessments with
        member me.Create name =
            let registerIdentity = RegisterId(Guid.NewGuid())
            let repository = registerRepo.Create registerIdentity
            let assessmentId = Guid.NewGuid()
            let assessmentIdentity = AssessmentId(Guid.NewGuid())
            let assessment = assessmentRepo.Create assessmentIdentity registerIdentity
            assessment.SetName name
            assessmentId

        member me.Get assessmentId =
            let identity = AssessmentId(assessmentId)
            let assessment = assessmentRepo.Open(identity).State
            let register = registerRepo.Open(assessment.RegisterIdentity).State
            constructAssessment assessment register

        member me.SetName assessmentId name =
            let identity = AssessmentId(assessmentId)
            let assessment = assessmentRepo.Open(identity)
            assessment.SetName name

        member me.AddExistingCandidate assessmentId candidateId =
            let identity = AssessmentId(assessmentId)
            let candidateIdentity = CandidateId(candidateId)
            let assessment = assessmentRepo.Open(identity)
            let register = registerRepo.Open(assessment.State.RegisterIdentity)
            let hasCandidate = register.State |> State.hasCandidate candidateIdentity
            if not hasCandidate then failWithCandidateNotFound candidateId
            assessment.AddCandidate candidateIdentity

        member me.AddNewCandidate assessmentId name =
            let identity = AssessmentId(assessmentId)
            let assessment = assessmentRepo.Open(identity)
            let register = registerRepo.Open(assessment.State.RegisterIdentity)
            let candidateId = Guid.NewGuid()
            let candidateIdentity = CandidateId(candidateId)
            register.AddCandidate candidateIdentity name
            candidateId

        member me.RemoveCandidate assessmentId candidateId =
            let identity = AssessmentId(assessmentId)
            let candidateIdentity = CandidateId(candidateId)
            assessmentRepo.Open(identity).RemoveCandidate candidateIdentity

        member me.ClearCandidateMark assessmentId candidateId =
            let identity = AssessmentId(assessmentId)
            let candidateIdentity = CandidateId(candidateId)
            assessmentRepo.Open(identity).SetCandidateResult candidateIdentity None

        member me.SetCandidateMark assessmentId candidateId mark =
            let identity = AssessmentId(assessmentId)
            let candidateIdentity = CandidateId(candidateId)
            assessmentRepo.Open(identity).SetCandidateResult candidateIdentity (Some(Mark(mark)))

        member me.SetCandidateResultMissingReason assessmentId candidateId resultMissingReason comment =
            let identity = AssessmentId(assessmentId)
            let candidateIdentity = CandidateId(candidateId)
            let parsedResultMissingReason = parseResultMissingReason resultMissingReason comment
            assessmentRepo.Open(identity).SetCandidateResult candidateIdentity (Some(parsedResultMissingReason))

module Factory =
    let Create =
        let createAssessmentCommandHandler = Assessments.CommandHandler.createCommandHandler
        let assessmentRepo = new Assessments.Repository.AssessmentRepository(createAssessmentCommandHandler)
        let createRegisterCommandHandler = Registers.CommandHandler.createCommandHandler
        let registerRepo = new Registers.Repository.RegisterRepository(createRegisterCommandHandler)
        new Assessments(assessmentRepo, registerRepo)
