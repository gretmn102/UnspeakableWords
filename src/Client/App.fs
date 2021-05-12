module App

open Elmish
open Elmish.Bridge
open Elmish.React

open Elmish.HMR

Program.mkProgram Index.init Index.update Index.view
|> Program.withBridgeConfig
    (Bridge.endpoint Shared.Remote.socketPath
     |> Bridge.withMapping Index.RC
     |> Bridge.withWhenDown Index.ConnectionLost)
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
|> Program.run
