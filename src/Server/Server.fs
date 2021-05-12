module Server

open Elmish.Bridge
open Elmish

open Shared

// open FsharpMyExtension
// open FsharpMyExtension.Either

open Prelude

type ServerMsg =
    | RS of RemoteServerMsg
    | Closed
type State =
    | Connected of User
    | Disconnected

let connections =
    ServerHub<State,ServerMsg,RemoteClientMsg>()

type History<'a> =
    {
        Get: unit -> 'a list
        Put: 'a -> unit
        ChangeName: string -> string -> unit
    }

let history =
    let mb =
        MailboxProcessor.Start
         (fun (mb:MailboxProcessor<Choice<AsyncReplyChannel<Msgs list>,Msgs, string*string>>) ->
             let rec loop l =
                async {
                    let! msg = mb.Receive()
                    match msg with
                    | Choice1Of3 r ->
                        r.Reply l
                        return! loop l
                    | Choice2Of3 m ->
                        return! loop (m::l |> List.truncate 50)
                    | Choice3Of3 (o,n) ->
                        return! loop (l |> List.map (function ClientMsg(c,m) when c = o -> ClientMsg(n,m)| m -> m))}
             loop [])
    {
        Get = fun () -> mb.PostAndReply (fun e -> Choice1Of3 e)
        Put = fun m -> mb.Post(Choice2Of3 m)
        ChangeName = fun o n -> mb.Post(Choice3Of3 (o,n))
    }

let sendCLient userId (msgs:RemoteClientMsg) =
    // TODO: найти вменяемый способ отправки, потому что-то это безобразие перебирает весь список, хотя мог бы послать конкретному пользователю
    let mutable success = false
    connections.SendClientIf
        (function
         | Connected u ->
            success <- true
            u.Name = userId
         | Disconnected -> false
        )
        msgs
    success

let update clientDispatch msg state =
    match msg with
    | Closed ->
        match state with
        | Disconnected -> ()
        | Connected u ->
            connections.BroadcastClient(RemoveUser u.Name)
            let msg =
                {
                    Time = System.DateTime.Now
                    Content = u.Name+" left the room"
                }
                |> SysMsg
            history.Put msg
            connections.BroadcastClient(AddMsg msg)
        Disconnected, Cmd.none
    | RS msg ->
        let nameInUse name =
            connections.GetModels()
            |> Seq.exists (function
                | Disconnected -> false
                | Connected { Name = n } -> n = name)
        match state, msg with
        | _, UsersConnected ->
            let users =
                connections.GetModels()
                |> Seq.choose (function
                    | Disconnected -> None
                    | Connected u -> Some u)
                |> Seq.toList
            clientDispatch (GetUsers users)
            clientDispatch (AddMsgs (history.Get()))
            state, Cmd.none
        | state, SetUser u ->
            match state with
            | Disconnected ->
                if nameInUse u.Name then
                    SysMsg {
                        Time = System.DateTime.Now
                        Content = "Name is in use"
                    }
                    |> AddMsg
                    |> clientDispatch

                    clientDispatch (LoginResult (Error YouAlreadyLogin))
                    state, Cmd.none
                else
                    connections.BroadcastClient(AddUser u)
                    let msg = SysMsg {Time=System.DateTime.Now; Content = u.Name + " joined the room"}
                    history.Put msg
                    connections.BroadcastClient(AddMsg msg)

                    let x = m.PostAndReply(fun r -> Login(u.Name, r))

                    x.Return
                    |> LoginResult
                    |> clientDispatch

                    x.PlayersMsgs
                    |> Map.iter (fun userId msgs ->
                        if u.Name <> userId then
                            let success = sendCLient userId (GameMsgs msgs)
                            // TODO: и что делать с сообщениями, если игрок вышел?
                            ()
                    )

                    x.PlayersMsgs.[u.Name]
                    |> GameMsgs
                    |> clientDispatch

                    let state =
                        match x.Return with
                        | Ok x ->
                            Connected u
                        | _ -> state
                    state, Cmd.none
            | Connected(_) ->
                clientDispatch (LoginResult (Error YouAlreadyLogin))
                state, Cmd.none

        | state, Shared.Move word ->
            match state with
            | Connected u ->
                let x = m.PostAndReply(fun r -> Move((u.Name, word), r))

                x.Return
                |> MoveResult
                |> clientDispatch

                x.PlayersMsgs
                |> Map.iter (fun userId msgs ->
                    if u.Name <> userId then
                        let success = sendCLient userId (GameMsgs msgs)
                        // TODO: и что делать с сообщениями, если игрок вышел?
                        ()
                )

                x.PlayersMsgs.[u.Name]
                |> GameMsgs
                |> clientDispatch
            | Disconnected ->
                Error (GetStateError YouAreNotLogin)
                |> MoveResult
                |> clientDispatch
            state, Cmd.none
        | Disconnected, SendMsg m ->
            state, Cmd.none
        | Connected u, SendMsg m ->
            if System.String.IsNullOrWhiteSpace m then
                ()
            else
                let msg = ClientMsg (u.Name,{Content=m;Time = System.DateTime.Now})
                history.Put msg
                connections.BroadcastClient(AddMsg msg)
            state, Cmd.none

let init (clientDispatch:Dispatch<RemoteClientMsg>) () =
    clientDispatch QueryConnected
    Disconnected, Cmd.none

open Saturn.Application
let server =
    Bridge.mkServer Remote.socketPath init update
    |> Bridge.withConsoleTrace
    |> Bridge.whenDown Closed
    |> Bridge.withServerHub connections
    |> Bridge.run Giraffe.server

let port =
    match System.Environment.GetEnvironmentVariable("PORT") with
    | null -> uint16 8086
    | port -> uint16 port
let publicPath = System.IO.Path.GetFullPath "./public"
let app =
  application {
    use_static publicPath
    use_router server
#if !DEBUG
    disable_diagnostics
#endif
    app_config Giraffe.useWebSockets
    url ("http://0.0.0.0:" + port.ToString() + "/")
  }

run app