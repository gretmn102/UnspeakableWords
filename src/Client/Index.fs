module Index

open Elmish
open Elmish.Bridge
open Shared
open Shared.Client

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't


type Connection =
    | Disconnected
    | Waiting
    | Connected of Result<unit, LoginError>

type State =
    {
        PlayerId: UserId
        Connection: Connection
        GameLog: int * Map<int, string>
        GameState: Client.GameState option
        PlayersTable: {| Remain:int; OtherPlayers:UserId Set |}
        SelectedLetters: LetterId list
        ConnectedUsers : User list
    }

type Msg =
    | RC of RemoteClientMsg
    | ConnectionLost

    | Login
    | ChangeUserId of string

    | RemoveNotify of int

    | Move

    | SelectedLettersAdd of LetterId
    | SelectedLettersRemove of LetterId

let init(): State * Cmd<Msg> =
    let state =
        {
            PlayerId = ""
            GameLog = 0, Map.empty
            GameState = None
            PlayersTable = {| Remain = 0; OtherPlayers = Set.empty |}
            SelectedLetters = []
            Connection = Disconnected
            ConnectedUsers = []
        }
    state, Cmd.none

let removeNotifyMs = 5 * 60 * 1000

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | RC rc ->
        match rc with
        | LoginResult res ->
            let state =
                match res with
                | Ok gameState ->
                    { state with
                        Connection = Connected (Ok ())
                        GameState = gameState
                    }
                | Error err ->
                    { state with
                        Connection = Connected (Error err)
                    }
            state, Cmd.none
        | QueryConnected ->
            match state.Connection with
            | Connected(Ok _) ->
                { Name = state.PlayerId; Color = Black }
                |> SetUser
                |> Bridge.Send
            | Waiting | Disconnected _ | Connected _ -> ()

            state, Cmd.bridgeSend UsersConnected
        | GameMsgs msgs ->
            let i, gameLog = state.GameLog
            let i', gameLog, state =
                msgs
                |> List.fold
                    (fun (i, gameLog, (state:State)) x ->
                        let state =
                            match x with
                            | GameStarted gameState ->
                                { state with
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
                                    match state.GameState with
                                    | Some gameState -> gameState
                                    | None ->
                                        failwith "GameState was null"
                                match x with
                                | NowTurn userId ->
                                    { state with
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
                                    { state with
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
                                | DiscardToDeck -> state
                                | WordSucc word ->
                                    let gameState =
                                        let currentPlayerId = gameState.CurrentPlayerMove
                                        if currentPlayerId = state.PlayerId then
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
                                    { state with
                                        GameState =
                                            { gameState with
                                                PlayedWords =
                                                    (word, gameState.CurrentPlayerMove)::gameState.PlayedWords
                                            }
                                            |> Some
                                    }
                                | CthulhuApproving(points, throwResult, res) ->
                                    match res with
                                    | Pass -> state
                                    | NotPass ->
                                        { state with
                                            GameState =
                                                let currentPlayerId = gameState.CurrentPlayerMove
                                                if currentPlayerId = state.PlayerId then
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
                                | InsaneCheck _ -> state
                                    // { st with
                                    //     GameState =
                                    //         { gameState with
                                    //             Discard = letters @ gameState.Discard
                                    //         }
                                    //         |> Some
                                    // }
                                | Discard letters ->
                                    { state with
                                        GameState =
                                            { gameState with
                                                Discard = letters @ gameState.Discard
                                            }
                                            |> Some
                                    }
                            | GameEnded -> state
                        i + 1, Map.add i (sprintf "%A" x) gameLog, state
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
                Cmd.batch xs
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

        | GetUsers(_)
        | AddUser(_)
        | RemoveUser(_)
        | AddMsg(_)
        | AddMsgs(_) as x ->
            printfn "not implemented yet1 '%A'" x
            state, Cmd.none
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
            Cmd.bridgeSend (Shared.Move state.SelectedLetters)
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
    | Login ->
        match state.PlayerId with
        | "" -> state, Cmd.none
        | name ->
            { state with Connection = Waiting }, Cmd.bridgeSend( SetUser { Name = name; Color = Black } )
    | ConnectionLost ->
        { state with Connection = Disconnected }, Cmd.none

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
        match state.Connection with
        | Connected x ->
            match x with
            | Ok () ->
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
                            | GameEnd -> str "GameEnd"

                            div [] [
                                str (sprintf "%A" gameState)
                            ]
                        ]
                    ]
                | None -> () //str "not started"
            | Error err ->
                loginBox
                div [] [str (sprintf "%A" err)]
        | Waiting ->
            div [] [
                Fa.i [ Fa.IconOption.Size Fa.ISize.Fa3x; Fa.Solid.Spinner; Fa.Spin ] []
            ]
        | Disconnected ->
            loginBox

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
