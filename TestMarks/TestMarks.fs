module TestMarks

type TestId = Guid
type PersonId = Guid

type Person =
    inherit System.IComparable
    abstract member Id : PersonId
    abstract member DisplayName : string

type Mark = decimal

type TestEvent =
    | PersonAdded of Person
    | MarkUpdated of Person * Mark
    | Deleted

let ApplyToAggregateRoot (event:TestEvent) (marksOption:Option<Map<Person, Mark>>) =
    match marksOption with
    | Some(marks) ->
        match event with
        | PersonAdded(person) -> Some(marks.Add(person, 0m))
        | MarkUpdated(person, mark) -> Some(marks.Remove(person).Add(person, mark))
        | Deleted -> None
    | None -> None



type Test =
    inherit Repos.Entity<TestId>
    abstract member Marks : Map<Person, Mark>
    abstract member AddPerson : Person -> unit
    abstract member UpdateMark : Person -> Mark -> unit
