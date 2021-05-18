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
    | DiscardHand of UserId * AsyncReplyChannel<Result<unit, MoveError> Res>

type GameState =
    | Mov of Abstr.Mov

type State =
    {
        Players: Map<UserId, unit>
        GameState: GameState option
        AbstrState: Abstr.State option
        EnglishNouns: string Set
        RussianNouns: string Set
        Language: Shared.Language
    }

let toClientGameState currPlayerId (abstrState:Abstr.State) language (gameState:GameState) : Client.GameState =
    let pls = abstrState.Pls
    {
        OtherPlayers =
            Map.remove currPlayerId pls
            |> Map.map (fun userId v ->
                {
                    Client.PlayerId = userId
                    Client.SanityPoints = v.SanityPoints
                    Client.Points = v.Points
                    Client.Hand = v.Hand.Count
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
        DeckCount =
            abstrState.Deck.Length
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
        Language = language
    }

let maxPlayers = 3

let throwDice =
    let r = System.Random()
    let n = Abstr.diceMax + 1
    fun () -> r.Next(1, n)

let justReturn x =
    {
        Return = x
        PlayersMsgs = Map.empty
    }

let turn state playersMsgs = function
    | Abstr.Move(abstrState, currPlayerIdMove, _) ->
        let playersMsgs =
            playersMsgs
            |> Map.map (fun userId' v ->
                GameResponse (NowTurn currPlayerIdMove)::v
            )

        { state with AbstrState = Some abstrState }, playersMsgs
    | Abstr.End abstrState ->
        let playersMsgs =
            playersMsgs
            |> Map.map (fun userId' v ->
                GameEnded::v
            )

        { state with AbstrState = Some abstrState }, playersMsgs

let draw userId state playersMsgs = function
    | Abstr.Draws(letters, f) ->
        let playersMsgs =
            let lettersCount = letters.Length
            playersMsgs
            |> Map.map (fun userId' msgs ->
                if userId = userId' then
                    GameResponse (TakeLetters letters)
                    :: msgs
                else
                    GameResponse (OtherTakeLetters lettersCount)
                    :: msgs
            )

        let res = f ()
        let state =
            { state with
                GameState = Some (Mov res)
            }
        turn state playersMsgs res
    | Abstr.DrawsWithDiscardReuse(lettersBefore, lettersAfter, f) ->
        let playersMsgs =
            let lettersBeforeCount = lettersBefore.Length
            let lettersAfterCount = lettersAfter.Length
            playersMsgs
            |> Map.map (fun userId' msgs ->
                if userId = userId' then
                    GameResponse (TakeLetters lettersAfter)
                    :: GameResponse DiscardToDeck
                    :: GameResponse (TakeLetters lettersBefore)
                    :: msgs
                else
                    GameResponse (OtherTakeLetters lettersAfterCount)
                    :: GameResponse DiscardToDeck
                    :: GameResponse (OtherTakeLetters lettersBeforeCount)
                    :: msgs
            )

        let res = f ()
        let state =
            { state with
                GameState = Some (Mov res)
            }
        turn state playersMsgs res
    | Abstr.RemovePlayerBecauseCardsIsLeft f ->
        let playersMsgs =
            playersMsgs
            |> Map.map (fun userId' v ->
                GameResponse RemovePlayerBecauseCardsIsLeft::v
            )

        let res = f ()
        let state =
            { state with
                GameState = Some (Mov res)
            }
        turn state playersMsgs res
    | Abstr.MoveWithoutDraw f ->
        let res = f ()
        let state =
            { state with
                GameState = Some (Mov res)
            }
        turn state playersMsgs res

let getThrow userId word state playersMsgs = function
    | Abstr.ThrowDice getThrowDiceResult ->
        let wordSuccess = getThrowDiceResult (throwDice ())

        match wordSuccess.SanityCheckResult with
        | Abstr.Pass draws ->
            let playersMsgs =
                playersMsgs
                |> Map.map (fun userId' v ->
                    GameResponse
                        (SanityCheck
                            (wordSuccess.WordPoints, wordSuccess.DiceThrowResult, Pass))::v
                )
            draw userId state playersMsgs draws

        | Abstr.NotPass insaneCheckResult ->
            let playersMsgs =
                playersMsgs
                |> Map.map (fun userId' v ->
                    GameResponse
                        (SanityCheck
                            (wordSuccess.WordPoints, wordSuccess.DiceThrowResult, NotPass))::v
                )
            match insaneCheckResult with
            | Abstr.NotInsane draws ->
                let playersMsgs =
                    playersMsgs
                    |> Map.map (fun userId' v ->
                        GameResponse (InsaneCheck NotInsane)::v
                    )
                draw userId state playersMsgs draws
            | Abstr.GotInsane(lettersToDiscard, f) ->
                let playersMsgs =
                    playersMsgs
                    |> Map.map (fun userId' v ->
                        GameResponse (Discard lettersToDiscard)::GameResponse (InsaneCheck Insane)::v
                    )
                let res = f ()
                let state =
                    { state with
                        GameState = Some (Mov res)
                    }
                turn state playersMsgs res

let act userId state (r:_ AsyncReplyChannel) fn =
    if Map.containsKey userId state.Players then
        match state.GameState with
        | Some gameState ->
            match gameState with
            | Mov mov ->
                match mov with
                | Abstr.Move(abstrState, currPlayer, moveType) ->
                    if currPlayer = userId then
                        fn moveType
                    else
                        r.Reply (justReturn (Error NotYourMove))
                        state
                | Abstr.End _ ->
                    r.Reply (justReturn (Error GameEndedError))
                    state
        | None ->
            r.Reply (justReturn (Error GameHasNotStartedYet))
            state
    else
        r.Reply (justReturn (Error (GetStateError YouAreNotLogin)))
        state

let exec state = function
    | Login(userId, r) ->
        let playersCount = Map.count state.Players
        if Map.containsKey userId state.Players then

            match state.AbstrState, state.GameState with
            | Some abstrState, Some gameState ->
                toClientGameState userId abstrState state.Language gameState
                |> Some
                |> Ok
                |> justReturn
                |> r.Reply
            | _ ->
                justReturn (Ok None)
                |> r.Reply

            state
        elif playersCount >= maxPlayers then
            justReturn (Error PlayersRecruited)
            |> r.Reply

            state
        else
            let playersMsgs =
                state.Players
                |> Map.map (fun _ v ->
                    [PlayerJoined userId]
                )
                |> Map.add userId []

            if playersCount + 1 = maxPlayers then // start the game
                let letters =
                    match state.Language with
                    | English ->
                        Init.allEnglishLetters
                    | Russian ->
                        Init.allRussianLetters

                let deck =
                    letters
                    |> List.map fst
                    |> List.shuffle

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
                let allLetters = Map.ofList letters

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
                    let nouns =
                        match state.Language with
                        | English -> state.EnglishNouns
                        | Russian -> state.RussianNouns
                    Abstr.loop
                        (fun word ->
                            word
                            |> List.map (
                                flip Map.find allLetters
                                >> fun l -> l.Name)
                            |> System.String.Concat
                            |> flip Set.contains nouns
                        )
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
                    let deckLength = deck.Length
                    playersMsgs
                    |> Map.map (fun currPlayerId v ->
                        match gameState with
                        | Abstr.Move(abstrState, currPlayerIdMove, _) ->
                            let x =
                                {
                                    Client.OtherPlayers =
                                        Map.remove currPlayerId pls
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
                                    Client.DeckCount = deckLength
                                    Client.PlayedWords = []
                                    Client.Discard = []
                                    Client.MoveStage =
                                        if currPlayerId = currPlayerIdMove then
                                            Client.StartingMove
                                        else
                                            Client.HasNotYourMoveYet
                                    Client.Language = state.Language
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

                { state with
                    GameState =
                        Mov gameState
                        |> Some
                    Players =
                        state.Players |> Map.add userId ()
                    AbstrState =
                        Some abstrState
                }
            else
                {
                    Return = Ok None
                    PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                }
                |> r.Reply

                { state with
                    Players =
                        state.Players |> Map.add userId ()
                }
    | Move((userId, word), r) ->
        act userId state r (fun moveType ->
            match moveType.PlayWord word with
            | Right x ->
                let state, playersMsgs =
                    match x with
                    | Abstr.WordSuccess next ->
                        let playersMsgs =
                            state.Players
                            |> Map.map (fun userId' _ ->
                                [GameResponse (WordSucc word)]
                            )
                        getThrow userId word state playersMsgs next
                    | Abstr.WordNotExist (pls, isApprove) ->
                        let playersMsgs =
                            state.Players
                            |> Map.map (fun userId' _ ->
                                [GameResponse (WordNotExist word)]
                            )
                        match isApprove false with
                        | Abstr.PlsNotWordApproved ->
                            state, playersMsgs
                        | Abstr.PlsWordApproved next ->
                            let playersMsgs =
                                playersMsgs
                                |> Map.map (fun userId' _ ->
                                    [GameResponse (WordSucc word)]
                                )
                            getThrow userId word state playersMsgs next
                {
                    Return = Ok ()
                    PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                }
                |> r.Reply

                state
            | Left err ->
                justReturn (Error (MovError err))
                |> r.Reply
                state
        )
    | DiscardHand(userId, r) ->
        act userId state r (fun moveType ->
            let discardedLetters, x = moveType.DiscardHand ()

            let state, playersMsgs =
                let playersMsgs =
                    state.Players
                    |> Map.map (fun userId' _ ->
                        [GameResponse (OtherDiscardHand discardedLetters)]
                    )

                draw userId state playersMsgs x
            {
                Return = Ok ()
                PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
            }
            |> r.Reply

            state
        )
let m =
    let filterWords words =
        words
        |> Array.filter (fun (word:string) ->
            let isValidLength =
                let l = word.Length
                1 < l && l <= 7
            let isValid =
                word
                |> String.forall (fun x ->
                    System.Char.IsLower x
                    && System.Char.IsLetter x
                )
            isValidLength && isValid
        )
    let st =
        {
            Players = Map.empty
            GameState = None
            AbstrState = None
            EnglishNouns =
                printfn "English dictionary loading..."
                let words =
                    let path =
                        #if DEBUG
                        @"../../paket-files/david47k/top-english-wordlists/top_english_nouns_mixed_500000.txt"
                        #else
                        "english_nouns.txt"
                        #endif
                    let words =
                        try
                            // let path = "paket-files/david47k/top-english-wordlists/top_english_nouns_mixed_500000.txt"
                            System.IO.File.ReadAllLines path
                        with e ->
                            printfn "%s" e.Message
                            [||]
                    filterWords words
                    |> Set.ofArray
                printfn "English dictionary loaded"
                words
            RussianNouns =
                printfn "Russian dictionary loading..."
                let words =
                    let path =
                        #if DEBUG
                        @"../../paket-files/Badestrand/russian-dictionary/nouns.csv"
                        #else
                        "russian_nouns.csv"
                        #endif
                    let words =
                        try
                            // let path = "paket-files/Badestrand/russian-dictionary/nouns.csv"
                            System.IO.File.ReadAllLines path
                        with e ->
                            printfn "%s" e.Message
                            [||]
                    words
                    |> Array.map (fun x -> x.Split '\t' |> Array.head)
                    |> filterWords
                    |> Set.ofArray
                printfn "Russian dictionary loaded"
                words
            Language = Russian
        }
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
        loop st
    )
