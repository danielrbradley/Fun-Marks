namespace InteropTest
{
    using System;

    using Services.Assessments;

    public class Program
    {
        static void Main(string[] args)
        {
            Services.Assessments.IAssessments assessmentsService = Services.Assessments.Factory.Create;
            var test1Id = assessmentsService.Create("Assessment 1");
            PrintState(assessmentsService, test1Id);
            assessmentsService.SetName(test1Id, "Updated Assessment");
            PrintState(assessmentsService, test1Id);
            var candidate1Id = assessmentsService.AddNewCandidate(test1Id, "Candidate 1");
            PrintState(assessmentsService, test1Id);
            assessmentsService.SetCandidateMark(test1Id, candidate1Id, 1);
            PrintState(assessmentsService, test1Id);
            assessmentsService.SetCandidateResultMissingReason(
                test1Id, candidate1Id, Services.Assessments.ResultMissingReason.Absent, "Off ill");
            PrintState(assessmentsService, test1Id);
            assessmentsService.ClearCandidateMark(test1Id, candidate1Id);
            PrintState(assessmentsService, test1Id);
            assessmentsService.RemoveCandidate(test1Id, candidate1Id);
            PrintState(assessmentsService, test1Id);
        }

        private static void PrintState(IAssessments assessmentsService, Guid assessmentId)
        {
            var state = assessmentsService.Get(assessmentId);
            Console.WriteLine("Identity: {0}", state.Identity);
            Console.WriteLine("Name: {0}", state.Name);
            Console.WriteLine("Candidates: ");
            foreach (var candidate in state.Candidates)
            {
                Console.WriteLine("  Identity: {0}", candidate.Identity);
                Console.WriteLine("  Name: {0}", candidate.Name);
                Console.WriteLine("  Mark: {0}", candidate.Mark);
                Console.WriteLine("  ResultMissingReason: {0}", candidate.ResultMissingReason);
                Console.WriteLine("  ResultMissingReasonComment: {0}", candidate.ResultMissingReasonComment);
            }
        }
    }
}
