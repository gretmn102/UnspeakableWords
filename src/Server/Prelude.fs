module Prelude
open Shared
open FsharpMyExtension
open FsharpMyExtension.ListZipperCircle2


type T =
    | Login of UserId * AsyncReplyChannel<Result<unit, LoginError>>
    | GetState of UserId * AsyncReplyChannel<Result<GameResponse GetStateResult list, GetStateError>>
    | Move of UserId * AsyncReplyChannel<Result<unit, MoveError>>

type GameState =
    {
        Players: UserId LZC
    }
type State =
    {
        Players: Map<UserId, GameResponse GetStateResult list>
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
                                let players' =
                                    players
                                    |> Seq.map (fun (KeyValue(userId, _)) -> userId)
                                    |> List.ofSeq
                                    |> LZC.ofList
                                let currPlayer =
                                    LZC.hole players'
                                { st with
                                    GameState =
                                        {
                                            Players = players'
                                        } |> Some
                                    Players =
                                        players
                                        |> Map.map (fun _ v ->
                                            GameResponse (NowTurn currPlayer)::GameStarted::v
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
                    | Move(userId, r) ->
                        if Map.containsKey userId st.Players then
                            match st.GameState with
                            | Some gameState ->
                                let players = LZC.next gameState.Players
                                let currPlayerId = LZC.hole players

                                r.Reply (Ok ())
                                { st with
                                    GameState = Some { gameState with Players = players }
                                    Players =
                                        st.Players
                                        |> Map.map (fun userId' v ->
                                            GameResponse (NowTurn currPlayerId)::v
                                        )
                                }
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
