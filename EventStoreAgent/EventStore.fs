module EventStoreAgent

type Query<'Command,'Reply> =
| Query of 'Command * AsyncReplyChannel<'Reply>

let createAgent initialState createEvent apply writeEvent =
    MailboxProcessor.Start <| fun inbox ->
        let rec Loop state = 
            async {
                let! query = inbox.Receive()
                match query with
                | Query(command, replyChannel) ->
                    let eventOption = createEvent command
                    match eventOption with
                    | Some(event) ->
                        let newState = state |> apply event
                        writeEvent event
                        replyChannel.Reply newState
                        do! Loop newState
                    | None ->
                        replyChannel.Reply state
                        do! Loop state
            }
        Loop initialState

type EventStoreComandHandler<'Command,'Reply> (eventStoreAgent:MailboxProcessor<Query<'Command, 'Reply>>) =
    member me.ExecuteAsync command =
        eventStoreAgent.PostAndAsyncReply (fun (replyChannel:AsyncReplyChannel<'Reply>) -> Query(command, replyChannel))
    member me.Execute command =
        eventStoreAgent.PostAndReply (fun (replyChannel:AsyncReplyChannel<'Reply>) -> Query(command, replyChannel))
    new(initialState, createEvent, apply, writeEvent) =
        EventStoreComandHandler<'Command,'Reply>(createAgent initialState createEvent apply writeEvent)
