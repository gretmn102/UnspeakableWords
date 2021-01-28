module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't
type GameState =
    {
        CurrentPlayerMove:UserId option
    }
type State =
    {
        PlayerId: string
        LoginResult: Result<unit, LoginError> Deferred
        GameLog: int * Map<int, GameResponse GetStateResult>
        GameState: GameState option
    }

type Msg =
    | Login
    | LoginResult of Result<unit, LoginError>
    | ChangeUserId of string

    | GetState of sleep:int
    | GetStateResult of Result<GameResponse GetStateResult list, GetStateError>
    | RemoveNotify of int

    | Move
    | MoveResult of Result<unit,MoveError>

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IApi>

let init(): State * Cmd<Msg> =
    let state =
        {
            LoginResult = HasNotStartedYet
            PlayerId = ""
            GameLog = 0, Map.empty
            GameState = None
        }
    state, Cmd.none
let removeNotifyMs = 5000
let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | GetState sleep ->
        let cmd =
            if sleep > 0 then
                let step x = async {
                    do! Async.Sleep sleep
                    return! todosApi.getState x
                }
                Cmd.OfAsync.perform step state.PlayerId GetStateResult
            else
                Cmd.OfAsync.perform todosApi.getState state.PlayerId GetStateResult
        state, cmd
    | GetStateResult res ->
        match res with
        | Ok x ->
            let i, gameLog = state.GameLog
            let i', gameLog, res =
                x
                |> List.fold
                    (fun (i, gameLog, (st:GameState option)) x ->
                        let st =
                            match x with
                            | GameStarted ->
                                match st with
                                | None ->
                                    {
                                        CurrentPlayerMove = None
                                    }
                                    |> Some
                                | x -> x
                            | WaitPlayers _
                            | PlayerJoined _
                            | PlayerLeaved _ -> st
                            | GameResponse x ->
                                match x with
                                | NowTurn userId ->
                                    match st with
                                    | Some x ->
                                        { x with
                                            CurrentPlayerMove = Some userId
                                        }
                                        |> Some
                                    | None ->
                                        {
                                            CurrentPlayerMove = Some userId
                                        }
                                        |> Some
                        i + 1, Map.add i x gameLog, st
                    )
                    (i, gameLog, state.GameState)
            let state =
                { state with
                    GameLog = i', gameLog
                    GameState = res
                }
            let cmd =
                let xs =
                    [i..i' - 1]
                    |> List.map (fun i ->
                        async {
                            do! Async.Sleep removeNotifyMs
                            return RemoveNotify i
                        }
                        |> Cmd.OfAsync.result
                    )
                Cmd.ofMsg (GetState 1000) :: xs
                |> Cmd.batch
            state, cmd
        | Error(errorValue) ->
            printfn "%A" errorValue
            state, Cmd.ofMsg (GetState 1000)
    | Login ->
        let cmd = Cmd.OfAsync.perform todosApi.login state.PlayerId LoginResult
        let state = { state with LoginResult = InProgress }
        state, cmd
    | LoginResult res ->
        let cmd =
            match res with
            | Ok () -> Cmd.ofMsg (GetState 0)
            | Error _ -> Cmd.none
        { state with LoginResult = Resolved res }, cmd
    | ChangeUserId userId ->
        { state with PlayerId = userId }, Cmd.none
    | RemoveNotify i ->
        let state =
            { state with
                GameLog =
                    let i', gameLog = state.GameLog
                    i', Map.remove i gameLog }
        state, Cmd.none
    | Move ->
        let cmd = Cmd.OfAsync.perform todosApi.move state.PlayerId MoveResult
        state, cmd
    | MoveResult x ->
        printfn "%A" x
        state, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let navBrand =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
    ]

let containerBox (state : State) (dispatch : Msg -> unit) =
    Box.box' [ ] [
        let loginBox =
            Field.div [ Field.HasAddons ] [
                Control.p [ Control.IsExpanded ] [
                    Input.text [
                        Input.Value state.PlayerId
                        Input.Placeholder "UserId"
                        Input.OnChange (fun x ->
                            ChangeUserId x.Value
                            |> dispatch)
                        Input.Props [
                            OnKeyDown (fun e ->
                                if e.key = "Enter" then
                                    dispatch Login
                            )
                        ]
                    ]
                ]
                Control.p [ ] [
                    Button.a [
                        Button.OnClick (fun _ -> dispatch Login)
                    ] [
                        Fa.i [ Fa.Solid.SignInAlt ] []
                    ]
                ]
            ]
        match state.LoginResult with
        | Resolved x ->
            match x with
            | Ok () -> ()
            | Error err ->
                loginBox
                div [] [str (sprintf "%A" err)]
        | InProgress ->
            div [] [
                Fa.i [ Fa.IconOption.Size Fa.ISize.Fa3x; Fa.Solid.Spinner; Fa.Spin ] []
            ]
        | HasNotStartedYet ->
            loginBox
        match state.GameState with
        | Some gameState ->
            match gameState.CurrentPlayerMove with
            | Some userId ->
                if userId = state.PlayerId then
                    Control.p [ ] [
                        Button.a [
                            Button.OnClick (fun _ -> dispatch Move)
                        ] [
                            Fa.i [ Fa.Solid.Walking ] []
                        ]
                    ]
            | None -> ()
        | None -> ()
    ]

let view (state : State) (dispatch : Msg -> unit) =
    Hero.hero [
        // Hero.Color IsPrimary
        Hero.IsFullHeight
        Hero.Props [
            Style [
                // Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                BackgroundSize "cover"
            ]
        ]
    ] [
        Hero.head [ ] [
            Navbar.navbar [ ] [
                Container.container [ ] [ navBrand ]
            ]
        ]

        Hero.body [ ] [
            Container.container [ ] [
                Column.column [
                    Column.Width (Screen.All, Column.Is6)
                    Column.Offset (Screen.All, Column.Is3)
                ] [
                    Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "UnspeakableWords" ]
                    containerBox state dispatch
                ]
            ]
            state.GameLog
            |> snd
            |> Seq.map (fun (KeyValue(i, x)) ->
                li [] [
                    Notification.notification [
                    ] [
                        Notification.delete [
                            Props [
                                OnClick (fun e -> RemoveNotify i |> dispatch)
                            ]
                        ] []
                        div [] [
                            str (sprintf "%A" x)
                        ]
                    ]
                ]
            )
            |> List.ofSeq
            |> ul [
                Style [
                        Position PositionOptions.Absolute
                        ZIndex 1
                        Bottom 0
                        Right 0
                ]
            ]
        ]
    ]
