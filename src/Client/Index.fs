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
        NotificationsVisible: bool
    }

type Msg =
    | RC of RemoteClientMsg
    | ConnectionLost

    | Login
    | ChangeUserId of string

    | RemoveNotify of int

    | Move
    | DiscardHand

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
            NotificationsVisible = false
        }
    state, Cmd.none

let removeNotifyMs = 1 * 60 * 1000

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
            let gameLogId, gameLog = state.GameLog
            let (gameLogId', gameLog), state =
                msgs
                |> List.fold
                    (fun ((gameLogId, gameLog), (state:State)) x ->
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
                                                DeckCount = gameState.DeckCount - letters.Length
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
                                | OtherTakeLetters lettersCount ->
                                    let currentPlayerId = gameState.CurrentPlayerMove
                                    let pls = gameState.OtherPlayers
                                    let p = pls.[currentPlayerId]
                                    { state with
                                        GameState =
                                            { gameState with
                                                DeckCount = gameState.DeckCount - lettersCount
                                                OtherPlayers =
                                                    Map.add
                                                        currentPlayerId
                                                        { p with
                                                            Hand = p.Hand + lettersCount
                                                        }
                                                        pls
                                            }
                                            |> Some
                                    }
                                | DiscardToDeck ->
                                    { state with
                                        GameState =
                                            { gameState with
                                                Discard = []
                                                DeckCount = gameState.DeckCount + gameState.Discard.Length
                                            }
                                            |> Some
                                    }
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
                                | SanityCheck(points, throwResult, res) ->
                                    { state with
                                        GameState =
                                            let currentPlayerId = gameState.CurrentPlayerMove
                                            if currentPlayerId = state.PlayerId then
                                                { gameState with
                                                    ClientPlayer =
                                                        let p = gameState.ClientPlayer
                                                        { p with
                                                            SanityPoints =
                                                                match res with
                                                                | Pass -> p.SanityPoints
                                                                | NotPass ->
                                                                    p.SanityPoints - 1
                                                            Points = p.Points + points
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
                                                                SanityPoints =
                                                                    match res with
                                                                    | Pass -> p.SanityPoints
                                                                    | NotPass ->
                                                                        p.SanityPoints - 1
                                                                Points = p.Points + points
                                                            }
                                                            pls
                                                }
                                            |> Some
                                    }
                                | InsaneCheck x ->
                                    match x with
                                    | NotInsane -> state
                                    | Insane ->
                                        { state with
                                            GameState =
                                                let currentPlayerId = gameState.CurrentPlayerMove
                                                if currentPlayerId = state.PlayerId then
                                                    { gameState with
                                                        ClientPlayer =
                                                            let p = gameState.ClientPlayer
                                                            { p with
                                                                Hand = []
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
                                                                    Hand = 0
                                                                }
                                                                pls
                                                    }
                                                |> Some
                                        }
                                | Discard letters ->
                                    { state with
                                        GameState =
                                            { gameState with
                                                Discard = letters @ gameState.Discard
                                            }
                                            |> Some
                                    }
                                | RemovePlayerBecauseCardsIsLeft -> state
                                | OtherDiscardHand letters ->
                                    { state with
                                        GameState =
                                            let gameState =
                                                { gameState with
                                                    Discard = letters @ gameState.Discard
                                                }

                                            let currentPlayerId = gameState.CurrentPlayerMove
                                            if currentPlayerId = state.PlayerId then
                                                { gameState with
                                                    ClientPlayer =
                                                        let p = gameState.ClientPlayer
                                                        { p with
                                                            Hand = []
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
                                                                Hand = 0
                                                            }
                                                            pls
                                                }
                                            |> Some
                                    }
                            | GameEnded ->
                                { state with
                                    GameState =
                                        state.GameState
                                        |> Option.map (fun x ->
                                            { x with MoveStage = GameEnd }
                                        )
                                }
                        printfn "%A" x
                        (gameLogId + 1, Map.add gameLogId (sprintf "%A" x) gameLog), state
                    )
                    ((gameLogId, gameLog), state)
            let state =
                { state with
                    GameLog = gameLogId', gameLog
                }
            let cmd =
                let xs =
                    [gameLogId..gameLogId' - 1]
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
    | DiscardHand ->
        let cmd =
            Cmd.bridgeSend Shared.DiscardHand
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

let getLetterById =
    let allLetters = Map.ofList Init.allLetters
    fun x -> Map.find x allLetters

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
                                        // word
                                        // |> List.map (getLetterById >> fun x -> x.Name)
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
                                        let isEnabled = not (List.isEmpty state.SelectedLetters)
                                        Button.Disabled (not isEnabled)
                                        Button.OnClick (fun _ -> if isEnabled then dispatch Move)
                                    ] [
                                        Fa.i [ Fa.Solid.Walking ] []
                                    ]
                                ]

                                Control.p [] [
                                    Button.a [
                                        Button.OnClick (fun _ -> dispatch DiscardHand)
                                    ] [
                                        Fa.i [ Fa.Solid.Download ] []
                                    ]
                                ]
                            | ApprovingWord ->
                                div [] [
                                    str "ApprovingWord"
                                ]
                            | GameEnd -> str "GameEnd"

                            Table.table [] [
                                tbody []
                                    [
                                        tr [] [
                                            th [] [ str "Name" ]
                                            th [] [ str "Hand" ]
                                            th [] [ str "Points" ]
                                            th [] [ str "SanityPoints" ]
                                        ]
                                        let p = gameState.ClientPlayer

                                        tr [] [
                                            td [] [
                                                str p.Name
                                            ]
                                            td [] [
                                                str (string p.Hand)
                                            ]
                                            td [] [
                                                str (string p.Points)
                                            ]
                                            td [] [
                                                str (string p.SanityPoints)
                                            ]
                                        ]
                                    ]
                            ]

                            Table.table [] [
                                tbody []
                                    (seq {
                                        tr [] [
                                            th [] [ str "UserId" ]
                                            th [] [ str "Hand" ]
                                            th [] [ str "Points" ]
                                            th [] [ str "SanityPoints" ]
                                        ]
                                        yield!
                                            gameState.OtherPlayers
                                            |> Seq.map (fun (KeyValue(userId, otherPlayer)) ->
                                                tr [] [
                                                    td [] [
                                                        str userId
                                                    ]
                                                    td [] [
                                                        str (string otherPlayer.Hand)
                                                    ]
                                                    td [] [
                                                        str (string otherPlayer.Points)
                                                    ]
                                                    td [] [
                                                        str (string otherPlayer.SanityPoints)
                                                    ]
                                                ]
                                            )
                                    })
                            ]

                            div [] [
                                str (sprintf "DeckCount: %A" gameState.DeckCount)
                            ]
                            div [] [
                                str (sprintf "Discard: %A" gameState.Discard)
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

            if state.NotificationsVisible then
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
