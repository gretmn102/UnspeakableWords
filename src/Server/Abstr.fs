module Abstr
open FsharpMyExtension
open FsharpMyExtension.ListZipperCircle2
open FsharpMyExtension.Either
open Shared

type State = {
    PlsCircle: PlayerId LZC
    Pls: Map<PlayerId, Player<LetterId Set>>
    Deck : LetterId list
    Discards : LetterId list
    /// Уже использованные слова
    UsedWords: Word Set
    UsedWordsBy: (Word * PlayerId) list
}

type Move = (unit -> Mov)
and Draws =
    | Draws of LetterId list * Move
    /// В колоде не хватило карт, чтобы восполнить руку. Отбой переворачивается и считается как колода.
    /// Первый список - карты со старой колоды, 2-ой из новой.
    | DrawsWithDiscardReuse of LetterId list * LetterId list * Move
    /// Колода с отбоем пуста, карты больше неоткуда брать, поэтому игрок исключается из игры
    | RemovePlayerBecauseCardsIsLeft of Move
    | MoveWithoutDraw of Move
and InsaneCheckResult =
    /// Спятил. Карты с руки отправляются в отбой.
    | GotInsane of LetterId list * Move
    | NotInsane of Draws
and SanityCheckResult =
    | Pass of Draws
    | NotPass of InsaneCheckResult
/// Первое значение - сколько нужно выбросить, второе - итог броска игральной кости.
/// Если игральная кость показала наибольшее значение - Ктулху одобрил слово, и ему совсем не важно, сколько там требовалось получить.
/// А если же x = y тогда ?
and SanityCheck =
    {|
        WordPoints:int
        DiceThrowResult:int
        SanityCheckResult:SanityCheckResult
    |}
and ThrowDice = ThrowDice of (int -> SanityCheck)
and PlsApproving =
    | PlsWordApproved of ThrowDice
    | PlsNotWordApproved
and WordRes =
    /// Слово отсутствует в словаре. Устраивается голосование между остальными игроками - допустимо слово или нет.
    | WordNotExist of PlayerId list * (bool -> PlsApproving)
    | WordSuccess of ThrowDice
and MoveType =
    {
        DiscardHand: unit -> LetterId list * Draws
        /// The input list must not be empty
        PlayWord: Word -> Either<MovError, WordRes>
    }
and Mov =
    | Move of State * PlayerId * MoveType
    | End of State

/// Наибольшее количество карт, которое может держать каждый игрок.
let handCap = 7

module List =
    let splitAt' n xs =
        let rec f n acc xs =
            if n > 0 then
                match xs with
                | x::xs ->
                    f (n - 1) (x::acc) xs
                | [] ->
                    n, (List.rev acc, [])
            else
                0, (List.rev acc, xs)
        f n [] xs
    let splitAtTests () =
        [
            splitAt' 0 [] = (0, ([], []))
            splitAt' 3 [1] = (2, ([1], []))
            splitAt' 3 [1; 2] = (1, ([1; 2], []))
            splitAt' 3 [1; 2; 3] = (0, ([1; 2; 3], []))
            splitAt' 3 [1..10] = (0, ([1; 2; 3], [4; 5; 6; 7; 8; 9; 10]))
        ] |> List.forall id


let draw n (p:Player<LetterId Set>) (st:State) next : Draws =
    match st.Deck, st.Discards with
    | [], [] ->
        if Set.isEmpty p.Hand then
            let st =
                { st with
                    PlsCircle =
                        LZC.removeL st.PlsCircle |> Option.get
                }
            RemovePlayerBecauseCardsIsLeft(next st)
        else
            MoveWithoutDraw(next st)
    | deck, discards ->
        let n, (letters, deck) = List.splitAt' n deck
        let hand =
            letters
            |> List.fold (flip Set.add) p.Hand
        if n > 0 then
            let _, (lettersAfter, deck) = List.splitAt' n (List.rev discards)

            let hand =
                lettersAfter
                |> List.fold (flip Set.add) hand
            let st =
                { st with
                    Deck = deck
                    Pls =
                        let p = { p with Hand = hand }
                        Map.add p.Name p st.Pls
                    Discards = []
                }
            DrawsWithDiscardReuse(letters, lettersAfter, next st)
        else
            let st =
                { st with
                    Pls =
                        let p = { p with Hand = hand }
                        Map.add p.Name p st.Pls
                    Deck = deck
                }
            Draws(letters, next st)

let diceMax = 20

let sanityCheck pts diceValue word (st:State) loop : SanityCheck =
    {|
        WordPoints = pts
        DiceThrowResult = diceValue
        SanityCheckResult =
            let p =
                let p = LZC.hole st.PlsCircle |> flip Map.find st.Pls
                { p with Points = p.Points + pts }

            let draw p =
                let p = { p with Hand = word |> List.fold (flip Set.remove) p.Hand }
                let st =
                    { st with
                        Pls = Map.add p.Name p st.Pls
                        UsedWords = Set.add word st.UsedWords
                        UsedWordsBy = (word, p.Name)::st.UsedWordsBy
                    }
                draw (handCap - Set.count p.Hand) p st loop

            if pts <= diceValue || diceValue = diceMax then
                Pass (draw p)
            else
                let reason = p.SanityPoints - 1
                let p = { p with SanityPoints = reason }
                if reason > 0 then
                    NotInsane (draw p)
                else
                    let cards = p.Hand |> Set.toList
                    let st =
                        { st with
                            PlsCircle =
                                LZC.removeL st.PlsCircle |> Option.get
                            Pls =
                                let p = { p with Hand = Set.empty }
                                Map.add p.Name p st.Pls
                            Discards =
                                cards @ st.Discards
                        }
                    GotInsane(cards, loop st)
                |> NotPass
    |}

let throwDice pts word (st:State) loop =
    ThrowDice(fun diceValue ->
        sanityCheck pts diceValue word st loop
    )

let chooseLetters wordExistInDic wordPts st next =
    let pId = LZC.hole st.PlsCircle
    let p = Map.find pId st.Pls

    {
        DiscardHand = fun () ->
            let cs = Set.toList p.Hand
            let st = { st with Discards = cs @ st.Discards }

            let p = { p with Hand = Set.empty }
            let st = { st with Pls = Map.add pId p st.Pls }
            cs, draw handCap p st next
        PlayWord =
            function
            | [] -> Left WordIsEmpty
            | word ->
                let ys = List.distinct word
                if word = ys then
                    // let rec f' acc = function
                    //     | [] -> Some acc
                    //     | x::xs ->
                    //         if Set.contains x acc then
                    //             f' (Set.remove x acc) xs
                    //         else None
                    // match f' p.Hand xs with
                    // | Some hand ->
                    if List.forall (flip Set.contains p.Hand) word then
                        if Set.contains word st.UsedWords then
                            Left WordWasUsed
                        else
                            if wordExistInDic word then
                                WordSuccess(throwDice (wordPts word) word st next)
                                |> Right
                            else
                                let rest = LZC.removeR st.PlsCircle |> Option.get |> LZC.toList
                                WordNotExist(rest, function
                                    | true ->
                                        PlsWordApproved(throwDice (wordPts word) word st next)
                                    | false -> PlsNotWordApproved
                                )
                                |> Right
                    else
                        Left LetterError
                else Left LetterError
    }

let rec loop wordExistInDic wordPts (st:State)  =
    let next st  =
        fun () ->
        { st with PlsCircle = LZC.next st.PlsCircle }
        |> loop wordExistInDic wordPts
    if LZC.isSingletone st.PlsCircle then End st
    else
        let pId = LZC.hole st.PlsCircle
        let p = Map.find pId st.Pls
        Move(st, pId, chooseLetters wordExistInDic wordPts st next)
