namespace InteropTest
{
    using System;

    using Services.Assessments;

    public class Program
    {
        public static void Main(string[] args)
        {
            IAssessments assessmentsService = Factory.Create;

            Console.WriteLine("Create Assessment 1.");
            var test1Id = assessmentsService.Create("Assessment 1");
            PrintState(assessmentsService, test1Id);

            Console.WriteLine("Rename to Updated Assessment.");
            assessmentsService.SetName(test1Id, "Updated Assessment");
            PrintState(assessmentsService, test1Id);

            Console.WriteLine("Add new Candidate 1.");
            var candidate1Id = assessmentsService.AddNewCandidate(test1Id, "Candidate 1");
            PrintState(assessmentsService, test1Id);

            Console.WriteLine("Set Candidate 1 mark to 1.");
            assessmentsService.SetCandidateMark(test1Id, candidate1Id, 1);
            PrintState(assessmentsService, test1Id);

            Console.WriteLine("Set Candidate 1 to Absent, Off ill.");
            assessmentsService.SetCandidateResultMissingReason(
                test1Id, candidate1Id, ResultMissingReason.Absent, "Off ill");
            PrintState(assessmentsService, test1Id);

            Console.WriteLine("Clear Candidate 1 result.");
            assessmentsService.ClearCandidateMark(test1Id, candidate1Id);
            PrintState(assessmentsService, test1Id);

            Console.WriteLine("Remote Candidate 1.");
            assessmentsService.RemoveCandidate(test1Id, candidate1Id);
            PrintState(assessmentsService, test1Id);

            Console.WriteLine("Press any key to exit.");
            Console.ReadKey(true);
        }

        private static void PrintState(IAssessments assessmentsService, Guid assessmentId)
        {
            var state = assessmentsService.Get(assessmentId);
            Console.WriteLine("Identity: {0}", state.Identity);
            Console.WriteLine("Name: {0}", state.Name);
            Console.WriteLine("Candidates: ");
            foreach (var candidate in state.Candidates)
            {
                Console.WriteLine(" Identity: {0}", candidate.Identity);
                Console.WriteLine("  Name: {0}", candidate.Name);
                Console.WriteLine("  Mark: {0}", candidate.Mark);
                Console.WriteLine("  ResultMissingReason: {0}", candidate.ResultMissingReason);
                Console.WriteLine("  ResultMissingReasonComment: {0}", candidate.ResultMissingReasonComment);
            }

            Console.WriteLine();
        }
    }
}
