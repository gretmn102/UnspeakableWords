module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Shared.Client

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type State =
    {
        PlayerId: UserId
        LoginResult: Result<unit, LoginError> Deferred
        GameLog: int * Map<int, string>
        GameState: Client.GameState option
        PlayersTable: {| Remain:int; OtherPlayers:UserId Set |}
        SelectedLetters: LetterId list
    }

type Msg =
    | Login
    | LoginResult of Result<unit, LoginError>
    | ChangeUserId of string

    | GetState of sleep:int
    | GetStateResult of Result<GetStateResult<GameResponse, Client.GameState> list, GetStateError>
    | RemoveNotify of int

    | Move
    | MoveResult of Result<unit,MoveError>

    | SelectedLettersAdd of LetterId
    | SelectedLettersRemove of LetterId

    | GetSet
    | GetSetResult of T

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
            PlayersTable = {| Remain = 0; OtherPlayers = Set.empty |}
            SelectedLetters = []
        }
    state, Cmd.none
let removeNotifyMs = 15000
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
            let i', gameLog, state =
                x
                |> List.fold
                    (fun (i, gameLog, (st:State)) x ->
                        let st =
                            match x with
                            | GameStarted gameState ->
                                { st with
                                    GameState = gameState |> Some }
                            | WaitPlayers count ->
                                { state with
                                    PlayersTable = {| state.PlayersTable with Remain = count |}
                                }
                            | PlayerJoined userId ->
                                { state with
                                    PlayersTable =
                                        let playersTable = state.PlayersTable
                                        {| playersTable with OtherPlayers = Set.remove userId playersTable.OtherPlayers |}
                                }
                            | PlayerLeaved userId ->
                                { state with
                                    PlayersTable =
                                        let playersTable = state.PlayersTable
                                        {| playersTable with OtherPlayers = Set.add userId playersTable.OtherPlayers |}
                                }
                            | GameResponse x ->
                                let gameState =
                                    match st.GameState with
                                    | Some gameState -> gameState
                                    | None ->
                                        failwith "GameState was null"
                                match x with
                                | NowTurn userId ->
                                    { st with
                                        GameState =
                                            { gameState with
                                                CurrentPlayerMove = userId
                                                MoveStage =
                                                    if state.PlayerId = userId then
                                                        StartingMove
                                                    else
                                                        HasNotYourMoveYet
                                            }
                                            |> Some
                                    }
                                | TakeLetters letters ->
                                    { st with
                                        GameState =
                                            { gameState with
                                                ClientPlayer =
                                                    let p = gameState.ClientPlayer
                                                    { p with
                                                        Hand =
                                                            letters
                                                            |> List.fold (fun st x -> x :: st) p.Hand
                                                    }
                                            }
                                            |> Some
                                    }
                                    // st
                                | DiscardToDeck -> st
                                | WordSucc word ->
                                    let gameState =
                                        let currentPlayerId = gameState.CurrentPlayerMove
                                        if currentPlayerId = st.PlayerId then
                                            { gameState with
                                                ClientPlayer =
                                                    let p = gameState.ClientPlayer
                                                    { p with
                                                        Hand =
                                                            let s = Set.ofList word
                                                            p.Hand |> List.filter (fun x -> not <| Set.contains x s)
                                                            // |> List.fold (fun st x -> Set.remove x st) p.Hand
                                                    }
                                            }
                                        else
                                            let pls = gameState.OtherPlayers
                                            let p = pls.[currentPlayerId]
                                            { gameState with
                                                OtherPlayers =
                                                    Map.add
                                                        currentPlayerId
                                                        { p with
                                                            Hand = p.Hand - word.Length
                                                        }
                                                        pls
                                            }
                                    { st with
                                        GameState =
                                            { gameState with
                                                PlayedWords =
                                                    (word, gameState.CurrentPlayerMove)::gameState.PlayedWords
                                            }
                                            |> Some
                                    }
                                | CthulhuApproving(points, throwResult, res) ->
                                    match res with
                                    | Pass -> st
                                    | NotPass ->
                                        { st with
                                            GameState =
                                                let currentPlayerId = gameState.CurrentPlayerMove
                                                if currentPlayerId = st.PlayerId then
                                                    { gameState with
                                                        ClientPlayer =
                                                            let p = gameState.ClientPlayer
                                                            { p with SanityPoints = p.SanityPoints - 1}
                                                    }
                                                else
                                                    let pls = gameState.OtherPlayers
                                                    let p = pls.[currentPlayerId]
                                                    { gameState with
                                                        OtherPlayers =
                                                            Map.add
                                                                currentPlayerId
                                                                { p with SanityPoints = p.SanityPoints - 1}
                                                                pls
                                                    }
                                                |> Some
                                        }
                                        // st
                                | InsaneCheck _ -> st
                                    // { st with
                                    //     GameState =
                                    //         { gameState with
                                    //             Discard = letters @ gameState.Discard
                                    //         }
                                    //         |> Some
                                    // }
                                | Discard letters ->
                                    { st with
                                        GameState =
                                            { gameState with
                                                Discard = letters @ gameState.Discard
                                            }
                                            |> Some
                                    }
                            | GameEnded -> st
                        i + 1, Map.add i (sprintf "%A" x) gameLog, st
                    )
                    (i, gameLog, state)
            let state =
                { state with
                    GameLog = i', gameLog
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
        let cmd =
            Cmd.OfAsync.perform todosApi.move (state.PlayerId, state.SelectedLetters) MoveResult
        let state =
            { state with
                SelectedLetters = []
                GameState =
                    state.GameState
                    |> Option.map (fun gameState ->
                        { gameState with
                            MoveStage = ApprovingWord
                        }
                    )
            }
        state, cmd
    | MoveResult x ->
        let state =
            match x with
            | Ok _ -> state
            | Error err ->
                let i, log = state.GameLog

                { state with
                    GameLog = i + 1, Map.add i (sprintf "%A" err) log }
        state, Cmd.none
    | SelectedLettersAdd letter ->
        let state =
            { state with
                SelectedLetters = state.SelectedLetters @ [letter] }
        state, Cmd.none
    | SelectedLettersRemove letter ->
        let state =
            { state with
                SelectedLetters = List.filter ((<>) letter) state.SelectedLetters }
        state, Cmd.none
    | GetSet ->
        let cmd =
            Cmd.OfAsync.perform todosApi.getSet () GetSetResult
        state, cmd
    | GetSetResult s ->
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

        Button.button [
            Button.OnClick (fun _ -> dispatch GetSet)
        ] [
            str "Get set"
        ]

        match state.GameState with
        | Some gameState ->
            Columns.columns [] [
                Column.column [] [
                    gameState.PlayedWords
                    |> List.map (fun (word, userId) ->
                        tr [] [
                            td [] [
                                word
                                |> System.String.Concat
                                |> str
                            ]
                            td [] [
                                str userId
                            ]
                        ]
                    )
                    |> fun xs ->
                        Table.table [] [
                            tbody [] xs
                        ]
                ]

                Column.column [] [
                    div [] [
                        str (sprintf "CurrentPlayerId: %s" gameState.CurrentPlayerMove)
                    ]

                    match gameState.MoveStage with
                    | HasNotYourMoveYet -> ()
                    | StartingMove ->
                        let s = Set.ofList state.SelectedLetters
                        gameState.ClientPlayer.Hand
                        |> List.choose (fun letter ->
                            if Set.contains letter s then None
                            else
                                Button.span [
                                    Button.OnClick (fun _ -> dispatch (SelectedLettersAdd letter))
                                ] [
                                    str (string letter)
                                ]
                                |> Some
                        )
                        |> div []

                        state.SelectedLetters
                        |> List.map (fun letter ->
                            Button.span [
                                Button.OnClick (fun _ -> dispatch (SelectedLettersRemove letter))
                            ] [
                                str (string letter)
                            ]
                        )
                        |> div []

                        Control.p [ ] [
                            Button.a [
                                Button.Disabled (List.isEmpty state.SelectedLetters)
                                Button.OnClick (fun _ -> dispatch Move)
                            ] [
                                Fa.i [ Fa.Solid.Walking ] []
                            ]
                        ]
                    | ApprovingWord ->
                        div [] [
                            str "ApprovingWord"
                        ]

                    div [] [
                        str (sprintf "%A" gameState)
                    ]
                ]
            ]
        | None -> () //str "not started"
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
                    // Column.Width (Screen.All, Column.Is6)
                    // Column.Offset (Screen.All, Column.Is3)
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
