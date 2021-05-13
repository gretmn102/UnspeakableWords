module Prelude
open Shared
open FsharpMyExtension
open FsharpMyExtension.Either
open FsharpMyExtension.ListZipperCircle2

type 'a Res =
    {
        Return: 'a
        PlayersMsgs: Map<UserId, GetStateResult<GameResponse, Client.GameState> list>
    }

type T =
    | Login of UserId * AsyncReplyChannel<Result<Client.GameState option, LoginError> Res>
    | Move of (UserId * Word) * AsyncReplyChannel<Result<unit, MoveError> Res>

type GameState =
    | Mov of Abstr.Mov

type State =
    {
        Players: Map<UserId, unit>
        GameState: GameState option
        AbstrState: Abstr.State option
    }

let toClientGameState currPlayerId (abstrState:Abstr.State) (gameState:GameState) : Client.GameState =
    let pls = abstrState.Pls
    {
        OtherPlayers =
            pls
            |> Map.map (fun userId v ->
                {
                    Client.PlayerId = userId
                    Client.SanityPoints = v.SanityPoints
                    Client.Points = v.Points
                    Client.Hand = Abstr.handCap
                }
            )
        CurrentPlayerMove =
            LZC.hole abstrState.PlsCircle
        ClientPlayer =
            let p = pls.[currPlayerId]
            {
                Name = p.Name
                Hand = Set.toList p.Hand
                Points = p.Points
                SanityPoints = p.SanityPoints
            }
        PlayedWords =
            abstrState.UsedWordsBy
        Discard =
            abstrState.Discards
        MoveStage =

            match gameState with
            | Mov m ->
                match m with
                | Abstr.Move(_, currPlayerIdMove, _) ->
                    if currPlayerId = currPlayerIdMove then
                        Client.StartingMove
                    else
                        Client.HasNotYourMoveYet
                | Abstr.End _ ->
                    Client.GameEnd
    }

let maxPlayers = 3

let exec st =
    let justReturn x =
        {
            Return = x
            PlayersMsgs = Map.empty
        }
    function
    | Login(userId, r) ->
        let playersCount = Map.count st.Players
        if Map.containsKey userId st.Players then

            match st.AbstrState, st.GameState with
            | Some abstrState, Some gameState ->
                toClientGameState userId abstrState gameState
                |> Some
                |> Ok
                |> justReturn
                |> r.Reply
            | _ ->
                justReturn (Ok None)
                |> r.Reply

            st
        elif playersCount >= maxPlayers then
            justReturn (Error PlayersRecruited)
            |> r.Reply

            st
        else
            let playersMsgs =
                st.Players
                |> Map.map (fun _ v ->
                    [PlayerJoined userId]
                )
                |> Map.add userId []

            if playersCount + 1 = maxPlayers then // start the game
                let deck = Init.allLetters |> List.map fst

                let playersMsgs, pls, deck =
                    playersMsgs
                    |> Map.fold
                        (fun (playersMsgs, pls, deck) playerId msgs ->
                            let letters, deck =
                                List.take Abstr.handCap deck, List.skip Abstr.handCap deck
                            let p =
                                {
                                    Player.Name = playerId
                                    Player.Hand = Set.ofList letters
                                    Player.Points = 0
                                    Player.SanityPoints = Init.sanityPoints
                                }
                            // let playersMsgs =
                            //     Map.add playerId (GameResponse (TakeLetters letters)::msgs) players
                            playersMsgs, Map.add playerId p pls, deck
                        )
                        (playersMsgs, Map.empty, deck)
                let allLetters = Map.ofList Init.allLetters

                let abstrState: Abstr.State =
                    {
                        PlsCircle =
                            playersMsgs
                            |> Seq.map (fun (KeyValue(userId, _)) -> userId)
                            |> List.ofSeq
                            |> LZC.ofList
                        Pls = pls
                        Deck = deck
                        Discards = []
                        UsedWords = Set.empty
                        UsedWordsBy = []
                    }

                let gameState =
                    Abstr.loop
                        (fun _ -> true)
                        (fun letters ->
                            letters
                            |> List.sumBy (fun cid ->
                                match Map.tryFind cid allLetters with
                                | Some x -> x.Points
                                | None -> 0
                            )
                        )
                        abstrState
                let playersMsgs =
                    playersMsgs
                    |> Map.map (fun currPlayerId v ->
                        match gameState with
                        | Abstr.Move(state, currPlayerIdMove, _) ->
                            let x =
                                {
                                    Client.OtherPlayers =
                                        pls
                                        |> Map.map (fun userId v -> // (KeyValue(userId, v))
                                            {
                                                Client.PlayerId = userId
                                                Client.SanityPoints = v.SanityPoints
                                                Client.Points = v.Points
                                                Client.Hand = Abstr.handCap
                                            }
                                        )
                                    Client.CurrentPlayerMove = currPlayerIdMove
                                    Client.ClientPlayer =
                                        let p = pls.[currPlayerId]
                                        {
                                            Name = p.Name
                                            Hand = Set.toList p.Hand
                                            Points = p.Points
                                            SanityPoints = p.SanityPoints
                                        }

                                    Client.PlayedWords = []
                                    Client.Discard = []
                                    Client.MoveStage =
                                        if currPlayerId = currPlayerIdMove then
                                            Client.StartingMove
                                        else
                                            Client.HasNotYourMoveYet
                                }
                            GameStarted x::v
                        | Abstr.End _ ->
                            GameEnded::v
                    )
                {
                    Return = Ok None
                    PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                }
                |> r.Reply

                { st with
                    GameState =
                        Mov gameState
                        |> Some
                    Players =
                        st.Players |> Map.add userId ()
                    AbstrState =
                        Some abstrState
                }
            else
                {
                    Return = Ok None
                    PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                }
                |> r.Reply

                { st with
                    Players =
                        st.Players |> Map.add userId ()
                }
    | Move((userId, word), r) ->
        if Map.containsKey userId st.Players then
            match st.GameState with
            | Some gameState ->
                match gameState with
                | Mov mov ->
                    match mov with
                    | Abstr.Move(abstrState, currPlayer, f) ->
                        if currPlayer = userId then
                            match f.PlayWord word with
                            | Right x ->
                                let st, playersMsgs =
                                    match x with
                                    | Abstr.WordSuccess((points, throwResult), x) ->
                                        let playersMsgs =
                                            st.Players
                                            |> Map.map (fun userId' v ->
                                                [GameResponse (WordSucc word)]
                                            )
                                        let turn st playersMsgs = function
                                            | Abstr.Move(abstrState, currPlayerIdMove, _) ->
                                                let playersMsgs =
                                                    playersMsgs
                                                    |> Map.map (fun userId' v ->
                                                        GameResponse (NowTurn currPlayerIdMove)::v
                                                    )

                                                { st with AbstrState = Some abstrState }, playersMsgs
                                            | Abstr.End abstrState ->
                                                let playersMsgs =
                                                    playersMsgs
                                                    |> Map.map (fun userId' v ->
                                                        GameEnded::v
                                                    )

                                                { st with AbstrState = Some abstrState }, playersMsgs
                                        let drawf playersMsgs = function
                                            | Abstr.Draws(letters, f) ->
                                                let playersMsgs =
                                                    let x = GameResponse (TakeLetters letters)::Map.find currPlayer playersMsgs
                                                    Map.add currPlayer x playersMsgs

                                                let res = f ()
                                                let state =
                                                    { st with
                                                        GameState = Some (Mov res)
                                                    }
                                                turn state playersMsgs res
                                            | Abstr.DrawsWithDiscardReuse(lettersBefore, lettersAfter, f) ->
                                                let playersMsgs =
                                                    let x = GameResponse (TakeLetters lettersBefore)::playersMsgs.[currPlayer]
                                                    Map.add currPlayer x playersMsgs
                                                let playersMsgs =
                                                    playersMsgs
                                                    |> Map.map (fun userId' v ->
                                                        GameResponse DiscardToDeck::v
                                                    )
                                                let playersMsgs =
                                                    let x = GameResponse (TakeLetters lettersAfter)::playersMsgs.[currPlayer]
                                                    Map.add currPlayer x playersMsgs
                                                let res = f ()
                                                let state =
                                                    { st with
                                                        GameState = Some (Mov res)
                                                    }
                                                turn state playersMsgs res
                                        match x with
                                        | Abstr.Pass draw ->
                                            let players =
                                                playersMsgs
                                                |> Map.map (fun userId' v ->
                                                    GameResponse (CthulhuApproving (points, throwResult, Pass))::v
                                                )
                                            drawf players draw

                                        | Abstr.NotPass insaneCheckResult ->
                                            let playersMsgs =
                                                playersMsgs
                                                |> Map.map (fun userId' v ->
                                                    GameResponse (CthulhuApproving (points, throwResult, NotPass))::v
                                                )
                                            match insaneCheckResult with
                                            | Abstr.NotInsane draw ->
                                                let playersMsgs =
                                                    playersMsgs
                                                    |> Map.map (fun userId' v ->
                                                        GameResponse (InsaneCheck NotInsane)::v
                                                    )
                                                drawf playersMsgs draw
                                            | Abstr.GotInsane(lettersToDiscard, f) ->
                                                let playersMsgs =
                                                    playersMsgs
                                                    |> Map.map (fun userId' v ->
                                                        GameResponse (Discard lettersToDiscard)::GameResponse (InsaneCheck NotInsane)::v
                                                    )
                                                let res = f ()
                                                let state =
                                                    { st with
                                                        GameState = Some (Mov res)
                                                    }
                                                turn state playersMsgs res
                                {
                                    Return = Ok ()
                                    PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                                }
                                |> r.Reply

                                st
                            | Left err ->
                                justReturn (Error (MovError err))
                                |> r.Reply
                                st
                        else
                            r.Reply (justReturn (Error NotYourMove))
                            st
                    | Abstr.End _ ->
                        r.Reply (justReturn (Error GameEndedError))
                        st
            | None ->
                r.Reply (justReturn (Error GameHasNotStartedYet))
                st
        else
            r.Reply (justReturn (Error (GetStateError YouAreNotLogin)))
            st

let m =
    MailboxProcessor.Start (fun mail ->
        let rec loop (st:State) =
            async {
                let! res = mail.Receive()
                let st =
                    try
                        exec st res
                    with err ->
                        printfn "%A" err
                        st

                return! loop st
            }
        let st =
            {
                Players = Map.empty
                GameState = None
                AbstrState = None
            }
        loop st
    )
