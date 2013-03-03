namespace Repos

type Entity<'Identity> =
    abstract member Identity : 'Identity

type Query<'Source,'Result> =
    abstract member Result: 'Source -> 'Result

type Repo<'Identity,'Entity,'Source> =
    abstract member Create : 'Identity -> 'Entity
    abstract member Open : 'Identity -> Option<'Entity>
    abstract member Query : 'Identity -> Query<'Source,'Result> -> 'Result

type SingletonRepo<'Entity,'Source> =
    abstract member Open : unit -> 'Entity
    abstract member Query : Query<'Source,'Result> -> 'Result
