module SharedActors

open System.Threading
open System.Collections.Concurrent

type 'a ISharedActor =
   abstract Post : msg:'a -> unit
   abstract PostAndReply<'b> : msgFactory:(('b -> unit) -> 'a) -> 'b

type 'a SharedMailbox() =
   let msgs = ConcurrentQueue<'a>()
   let mutable isStarted = false
   let mutable msgCount = 0
   let mutable react = Unchecked.defaultof<_>
   let mutable currentMessage = Unchecked.defaultof<_>

   let rec execute(isFirst) =

      let inline consumeAndLoop() =
         react currentMessage
         currentMessage <- Unchecked.defaultof<_>
         let newCount = Interlocked.Decrement &msgCount
         if newCount <> 0 then execute false

      if isFirst then consumeAndLoop()
      else
         let hasMessage = msgs.TryDequeue(&currentMessage)
         if hasMessage then consumeAndLoop()
         else 
            Thread.SpinWait 20
            execute false
   
   member __.Receive(callback) = 
      isStarted <- true
      react <- callback

   member __.Post msg =
      while not isStarted do Thread.SpinWait 20
      let newCount = Interlocked.Increment &msgCount
      if newCount = 1 then
         currentMessage <- msg
         // Might want to schedule this call on another thread.
         execute true
      else msgs.Enqueue msg
   
   member __.PostAndReply msgFactory =
      let value = ref Unchecked.defaultof<'b>
      use onReply = new AutoResetEvent(false)
      let msg = msgFactory (fun x ->
         value := x
         onReply.Set() |> ignore<bool>
      )
      __.Post msg
      onReply.WaitOne() |> ignore<bool>
      !value


   interface 'a ISharedActor with
      member __.Post msg = __.Post msg
      member __.PostAndReply msgFactory = __.PostAndReply msgFactory

module SharedActor =
   let Start f =
      let mailbox = new SharedMailbox<_>()
      f mailbox
      mailbox :> _ ISharedActor