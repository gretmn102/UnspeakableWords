module Prelude
open Shared
open FsharpMyExtension
open FsharpMyExtension.Either
open FsharpMyExtension.ListZipperCircle2


type T =
    | Login of UserId * AsyncReplyChannel<Result<unit, LoginError>>
    | GetState of UserId * AsyncReplyChannel<Result<GetStateResult<GameResponse, Client.GameState> list, GetStateError>>
    | Move of (UserId * Word) * AsyncReplyChannel<Result<unit, MoveError>>

type GameState =
    | Mov of Abstr.Mov

type State =
    {
        Players: Map<UserId, GetStateResult<GameResponse, Client.GameState> list>
        GameState: GameState option
    }

let maxPlayers = 3
let m =
    MailboxProcessor.Start (fun mail ->
        let rec loop (st:State) =
            async {
                let! res = mail.Receive()
                let st =
                    match res with
                    | Login(userId, r) ->
                        let playersCount = Map.count st.Players
                        if Map.containsKey userId st.Players then
                            // r.Reply (Ok ())
                            // match st.GameState with
                            // | Some x ->
                            //     x.

                            r.Reply (Error YouAlreadyLogin)
                            st
                        elif playersCount >= maxPlayers then
                            r.Reply (Error PlayersRecruited)
                            st
                        else
                            r.Reply (Ok ())

                            let players =
                                st.Players
                                |> Map.map (fun _ v ->
                                    PlayerJoined userId::v
                                )
                            let players =
                                players |> Map.add userId []

                            if playersCount + 1 = maxPlayers then
                                let deck = Init.allLetters |> List.map fst

                                let players, pls, deck =
                                    players
                                    |> Map.fold
                                        (fun (players, pls, deck) playerId msgs ->
                                            let letters, deck =
                                                List.take Abstr.handCap deck, List.skip Abstr.handCap deck
                                            let p =
                                                {
                                                    Player.Name = playerId
                                                    Player.Hand = Set.ofList letters
                                                    Player.Points = 0
                                                    Player.SanityPoints = Init.sanityPoints
                                                }
                                            // let players =
                                            //     Map.add playerId (GameResponse (TakeLetters letters)::msgs) players
                                            players, Map.add playerId p pls, deck
                                        )
                                        (players, Map.empty, deck)
                                let allLetters = Map.ofList Init.allLetters
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
                                        {
                                            PlsCircle =
                                                players
                                                |> Seq.map (fun (KeyValue(userId, _)) -> userId)
                                                |> List.ofSeq
                                                |> LZC.ofList
                                            Pls = pls
                                            Deck = deck
                                            Discards = []
                                            UsedWords = Set.empty
                                        }

                                { st with
                                    GameState =
                                        Mov gameState
                                        |> Some
                                    Players =
                                        players
                                        |> Map.map (fun currPlayerId v ->
                                            match gameState with
                                            | Abstr.Move(currPlayerIdMove, _) ->
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
                                                            // { p with Hand = Set.empty }
                                                            // p
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
                                    }
                            else
                                { st with Players = players }
                    | GetState(userId, r) ->
                        match Map.tryFind userId st.Players with
                        | Some req ->
                            r.Reply (Ok (List.rev req))
                            { st with
                                Players = Map.add userId [] st.Players }
                        | None ->
                            r.Reply (Error YouAreNotLogin)
                            st
                    | Move((userId, word), r) ->
                        if Map.containsKey userId st.Players then
                            match st.GameState with
                            | Some gameState ->
                                match gameState with
                                | Mov mov ->
                                    match mov with
                                    | Abstr.Move(currPlayer, f) ->
                                        if currPlayer = userId then
                                            match f.PlayWord word with
                                            | Right x ->
                                                r.Reply (Ok ())

                                                match x with
                                                | Abstr.WordSuccess((points, throwResult), x) ->
                                                    let players =
                                                        st.Players
                                                        |> Map.map (fun userId' v ->
                                                            GameResponse (WordSucc word)::v
                                                        )
                                                    let turn st = function
                                                        | Abstr.Move(currPlayerIdMove, _) ->
                                                            { st with
                                                                Players =
                                                                    st.Players
                                                                    |> Map.map (fun userId' v ->
                                                                        GameResponse (NowTurn currPlayerIdMove)::v
                                                                    )
                                                            }
                                                        | Abstr.End _ ->
                                                            { st with
                                                                Players =
                                                                    st.Players
                                                                    |> Map.map (fun userId' v ->
                                                                        GameEnded::v
                                                                    )
                                                            }
                                                    let drawf players = function
                                                        | Abstr.Draws(letters, f) ->
                                                            let players =
                                                                let x = GameResponse (TakeLetters letters)::Map.find currPlayer players
                                                                Map.add currPlayer x players

                                                            let res = f ()
                                                            let state =
                                                                { st with
                                                                    Players = players
                                                                    GameState = Some (Mov res)
                                                                }
                                                            turn state res
                                                        | Abstr.DrawsWithDiscardReuse(lettersBefore, lettersAfter, f) ->
                                                            let players =
                                                                let x = GameResponse (TakeLetters lettersBefore)::players.[currPlayer]
                                                                Map.add currPlayer x players
                                                            let players =
                                                                players
                                                                |> Map.map (fun userId' v ->
                                                                    GameResponse DiscardToDeck::v
                                                                )
                                                            let players =
                                                                let x = GameResponse (TakeLetters lettersAfter)::players.[currPlayer]
                                                                Map.add currPlayer x players
                                                            let res = f ()
                                                            let state =
                                                                { st with
                                                                    Players = players
                                                                    GameState = Some (Mov res)
                                                                }
                                                            turn state res
                                                    match x with
                                                    | Abstr.Pass draw ->
                                                        let players =
                                                            players
                                                            |> Map.map (fun userId' v ->
                                                                GameResponse (CthulhuApproving (points, throwResult, Pass))::v
                                                            )
                                                        drawf players draw

                                                    | Abstr.NotPass insaneCheckResult ->
                                                        let players =
                                                            players
                                                            |> Map.map (fun userId' v ->
                                                                GameResponse (CthulhuApproving (points, throwResult, NotPass))::v
                                                            )
                                                        match insaneCheckResult with
                                                        | Abstr.NotInsane draw ->
                                                            let players =
                                                                players
                                                                |> Map.map (fun userId' v ->
                                                                    GameResponse (InsaneCheck NotInsane)::v
                                                                )
                                                            drawf players draw
                                                        | Abstr.GotInsane(lettersToDiscard, f) ->
                                                            let players =
                                                                players
                                                                |> Map.map (fun userId' v ->
                                                                    GameResponse (Discard lettersToDiscard)::GameResponse (InsaneCheck NotInsane)::v
                                                                )
                                                            let res = f ()
                                                            let state =
                                                                { st with
                                                                    Players = players
                                                                    GameState = Some (Mov res)
                                                                }
                                                            turn state res
                                            | Left err ->
                                                r.Reply (Error (MovError err))
                                                st
                                        else
                                            r.Reply (Error NotYourMove)
                                            st
                                    | Abstr.End _ ->
                                        r.Reply (Error GameEndedError)
                                        st
                            | None ->
                                r.Reply (Error GameHasNotStartedYet)
                                st
                        else
                            r.Reply (Error (GetStateError YouAreNotLogin))
                            st
                return! loop st
            }
        let st =
            {
                Players = Map.empty
                GameState = None
            }
        loop st
    )
