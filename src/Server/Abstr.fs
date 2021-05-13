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
    | DeckIsOver of Move
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
and CthulhuApproving = (int*int) * SanityCheckResult
and PlsApproving =
    | PlsWordApproved of CthulhuApproving
    | PlsNotWordApproved
and WordRes =
    /// Слово отсутствует в словаре. Устраивается голосование между остальными игроками - допустимо слово или нет.
    | WordNotExist of PlayerId list * (bool -> PlsApproving)
    | WordSuccess of CthulhuApproving
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

let draw n (p:Player<LetterId Set>) (st:State) next : Draws =
    let rec f (hand, handList) n deck =
        if n > 0 then
            match deck with
            | [] ->
                let splitAt n xs =
                    let rec f n acc xs =
                        if n > 0 then
                            match xs with
                            | x::xs ->
                                f (n - 1) (x::acc) xs
                            | [] -> List.rev acc, []
                        else
                            List.rev acc, xs
                    f n [] xs
                match st.Discards with
                | [] ->
                    let st =
                        { st with
                            PlsCircle = LZC.removeR st.PlsCircle |> Option.get
                        }
                    DeckIsOver(next st)
                | discards ->
                    let cardFromDeck, deck = splitAt n (List.rev discards)

                    let hand =
                        cardFromDeck
                        |> List.fold
                            (fun hand x ->
                                Set.add x hand)
                            hand
                    let p = { p with Hand = hand }
                    let st =
                        { st with
                            Deck = deck
                            Pls = Map.add p.Name p st.Pls
                            Discards = []
                        }
                    DrawsWithDiscardReuse(List.rev handList, cardFromDeck, next st)
            | x::xs ->
                f (Set.add x hand, x::handList) (n-1) xs
        else
            let p = { p with Hand = hand }
            let st = {
                st with
                    Pls = Map.add p.Name p st.Pls
                    Deck = deck
                }
            Draws(List.rev handList, next st)
    f (p.Hand, []) n st.Deck

let diceMax = 20
let throwDice =
    let r = System.Random()
    let n = diceMax + 1
    fun () -> r.Next(1, n)
let cthulhuApproving pts word (st:State) loop : CthulhuApproving =
    let diceVal = throwDice ()
    let p = LZC.hole st.PlsCircle |> flip Map.find st.Pls

    let f p =
        let p = { p with Hand = word |> List.fold (flip Set.remove) p.Hand }
        let st =
            { st with
                Pls = Map.add p.Name p st.Pls
                UsedWords = Set.add word st.UsedWords
                UsedWordsBy = (word, p.Name)::st.UsedWordsBy
            }
        draw (handCap - Set.count p.Hand) p st loop

    if pts <= diceVal || diceVal = diceMax then
        Pass (f p)
    else
        let reason = p.SanityPoints - 1
        let p = { p with SanityPoints = reason }
        if reason > 0 then
            NotInsane (f p)
        else
            let xs = p.Hand |> Set.toList
            let st =
                { st with
                    Pls = Map.add p.Name p st.Pls
                    Discards = xs @ st.Discards
                }
            GotInsane(xs, loop st)
        |> NotPass
    |> fun x -> (pts, diceVal), x
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
                        elif wordExistInDic word then
                            WordSuccess(cthulhuApproving (wordPts word) word st next)
                            |> Right
                        else
                            let rest = LZC.removeR st.PlsCircle |> Option.get |> LZC.toList
                            WordNotExist(rest, function
                                | true ->
                                    PlsWordApproved(cthulhuApproving (wordPts word) word st next)
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
