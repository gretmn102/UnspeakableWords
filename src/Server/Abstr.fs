module Abstr
open FsharpMyExtension
open FsharpMyExtension.ListZipperCircle2


type LetterT = char
type LetterId = int
type Letter = { Name:LetterT; Points:int; Count:int }

type PlayerId = string
type LetterUniq = LetterT * LetterId
type Player = {
    Name: PlayerId
    Hand: LetterUniq Set
    Points: int
    Reason: int
}

type State = {
    PlsCircle: PlayerId LZC
    Pls: Map<PlayerId, Player>
    Deck : LetterUniq list
    Discards : LetterUniq list
    /// Уже использованные слова
    UsedWords: string Set
}

type PlsApproving =
    | PlsWordApproved of CthulhuApproving
    | PlsNotWordApproved of ChooseLetters
and WordRes =
    // /// Одно из указанных букв отсутствует на руке.
    | LetterError of ChooseLetters
    | WordWasUsed of ChooseLetters
    | ChoosedDiscards of LetterUniq list * Draws
    /// Слово отсутствует в словаре. Устраивается голосование между остальными игроками - допустимо слово или нет.
    | WordNotExist of PlayerId list * (bool -> PlsApproving)
    | WordSuccess of CthulhuApproving
/// Если список пуст - у игрока сбрасываются карты и он добирает по-новой.
and ChooseLetters = (LetterUniq list -> WordRes)
and Mov =
    | Move of PlayerId * ChooseLetters
    | End of State
/// избавление от рекурсии
and Move = (unit -> Mov)
and Draws =
    | Draws of LetterUniq list * Move
    /// В колоде не хватило карт, чтобы восполнить руку. Отбой переворачивается и считается как колода.
    /// Первый список - карты со старой колоды, 2-ой из новой.
    | DrawsWithDiscardReuse of LetterUniq list * LetterUniq list * Move
and InsanityResult =
    /// Спятил. Карты с руки отправляются в отбой.
    | GotInsane of LetterUniq list * Move
    | NotInsane of Draws
/// Первое значение - сколько нужно выбросить, второе - итог броска игральной кости.
/// Если игральная кость показала наибольшее значение - Ктулху одобрил слово, и ему совсем не важно, сколько там требовалось получить.
/// А если же x = y тогда ?
and CthulhuApproving = (int*int) * InsanityResult
/// Наибольшее количество карт, которое может держать каждый игрок.
let handCap = 7

let draw n (p:Player) (st:State) next : Draws =
    let rec f (hand, handList) n deck =
        if n <= 0 then // достаточно '=', но кто его знает.
            let p = { p with Hand = hand }
            let st = {
                st with
                    Pls = Map.add p.Name p st.Pls
                    Deck = deck
                }
            Draws(List.rev handList, next st)
        else
            match deck with
            | [] ->
                let cardFromDeck, deck = List.splitAt n (List.rev st.Discards)
                let hand =
                    cardFromDeck
                    |> List.fold (fun hand x ->
                                Set.add x hand )
                            hand
                let p = { p with Hand = hand }
                let st = {
                    st with
                        Deck = deck
                        Pls = Map.add p.Name p st.Pls
                        Discards = []
                    }
                DrawsWithDiscardReuse(List.rev handList, cardFromDeck, next st)
            | x::xs ->
                f (Set.add x hand, x::handList) (n-1) xs
    f (p.Hand, []) n st.Deck

let diceMax = 20
let throwDice =
    let r = System.Random()
    let n = diceMax + 1
    fun () -> r.Next(1, n)
let cthulhuApproving pts (st:State) loop : CthulhuApproving =
    let diceVal = throwDice ()
    let p = LZC.hole st.PlsCircle |> flip Map.find st.Pls
    if pts <= diceVal || diceVal = diceMax then
        let draw = draw handCap p st loop
        NotInsane draw
    else
        let reason = p.Reason - 1
        let p = { p with Reason = reason }
        if reason > 0 then
            let draw = draw handCap p st loop
            NotInsane draw
        else
            let xs = p.Hand |> Set.toList
            let st = {
                st with
                    Pls = Map.add p.Name p st.Pls
                    Discards = xs @ st.Discards
                }
            InsanityResult.GotInsane(xs, loop st)
    |> fun x -> (pts, diceVal), x
let chooseLetters wordExistInDic wordPts st next =
    let pId = LZC.hole st.PlsCircle
    let p = Map.find pId st.Pls
    let rec f = function
        | [] ->
            let cs = Set.toList p.Hand
            let st = { st with Discards = cs @ st.Discards }

            let p = { p with Hand = Set.empty }
            let st = { st with Pls = Map.add pId p st.Pls }
            ChoosedDiscards(cs,
                draw handCap p st next
            ) //|> Some
        | xs ->
            let ys = List.distinct xs
            if xs = ys then

                // let rec f' acc = function
                //     | [] -> Some acc
                //     | x::xs ->
                //         if Set.contains x acc then
                //             f' (Set.remove x acc) xs
                //         else None
                // match f' p.Hand xs with
                // | Some hand ->
                if List.forall (flip Set.contains p.Hand) xs then
                    let word =
                        xs |> List.map fst //>> System.Char.ToLower)
                        |> System.String.Concat
                    if Set.contains word st.UsedWords then
                        WordWasUsed f
                    elif wordExistInDic word then
                    // let p = { p with Hand = xs |> List.fold (flip Set.remove) p.Hand }
                        // let p = { p with Hand = hand }
                        // let st = { st with Pls = Map.add pId p st.Pls }
                        WordSuccess(cthulhuApproving (wordPts xs) st next)
                    else
                        let rest = LZC.removeR st.PlsCircle |> Option.get |> LZC.toList
                        WordNotExist(rest, function
                            | true ->
                                PlsWordApproved(cthulhuApproving (wordPts xs) st next)
                            | false -> PlsNotWordApproved f
                        )
                else
                // | None ->
                    LetterError f
            else LetterError f
    f
let rec loop wordExistInDic wordPts (st:State)  =
    let next st  =
        fun () ->
        { st with PlsCircle = LZC.next st.PlsCircle }
        |> loop wordExistInDic wordPts
    if LZC.isSingletone st.PlsCircle then End st
    else
        let pId = LZC.hole st.PlsCircle
        let p = Map.find pId st.Pls
        Move(pId, chooseLetters wordExistInDic wordPts st next)

let letters =
    [('A', 5, 10); ('B', 5, 2); ('C', 0, 2); ('D', 2, 3); ('E', 4, 10);
     ('F', 3, 2);  ('G', 2, 2); ('H', 4, 3); ('I', 4, 9); ('J', 2, 1);
     ('K', 3, 1);  ('L', 1, 5); ('M', 3, 3); ('N', 2, 5); ('O', 0, 8);
     ('P', 3, 2);  ('Q', 2, 1); ('R', 4, 5); ('S', 0, 5); ('T', 2, 5);
     ('U', 0, 4);  ('V', 1, 2); ('W', 3, 2); ('X', 4, 1); ('Y', 3, 2);
     ('Z', 2, 1)]
    |> List.map (fun (name, pts, count) -> { Name = name; Points = pts; Count = count})
