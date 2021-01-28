module Abstr.Tests
open Expecto

open FsharpMyExtension
#if INTERACTIVE
#load @"..\..\src\Server\Abstr.fs"
#endif
open Abstr
open FsharpMyExtension.ListZipperCircle2

let DrawTest =
    testList "DrawTest" [
        testCase "DrawsWithDiscardReuse 1 case" <| fun () ->
            let p = { Name = "0"; Hand = Set.empty; Points = 0; Reason = 0 }
            let letter = 'a', 0
            let st = {
                PlsCircle = LZC.ofList ["0"]
                Pls = Map.empty
                Deck = []
                Discards = [letter]
                UsedWords = Set.empty
            }
            let exp =
                {st with
                    Pls = Map[ p.Name, {p with Hand = Set[letter]}]
                    Discards = []
                }
            let act =
                match draw 1 p st (fun st () -> End st) with
                | DrawsWithDiscardReuse([],[x], f) when x = letter ->
                    match f() with
                    | End st -> st
                    | x -> failwith "2"
                | x -> failwith "1"
            Expect.equal act exp ""
        testCase "DrawsWithDiscardReuse 2 return" <| fun () ->
            let p = { Name = "0"; Hand = Set.empty; Points = 0; Reason = 0 }
            let letters = ['a'..'g'] |> List.mapi (fun i x -> x, i)

            let st = {
                PlsCircle = LZC.ofList ["0"]
                Pls = Map.empty
                Deck = List.take 3 letters
                Discards = List.skip 3 letters
                UsedWords = Set.empty
            }
            let n = 5
            // let l1, l2 = List.splitAt n letters
            let deck = List.rev st.Discards
            let rest = List.take (n - 3) deck
            let exp =
                {
                    st with
                        Pls = Map[ p.Name, {p with Hand = Set.ofList (st.Deck @ rest) }]
                        Deck = deck |> List.skip (n-3)
                        Discards = []
                }
            let act =
                match draw n p st (fun st () -> End st) with
                | DrawsWithDiscardReuse(xs,ys, g) ->
                    if st.Deck <> xs then failwithf "exp:\n%A\nact:\n%A" st.Deck xs
                    if rest <> ys then failwithf "exp:\n%A\nact:\n%A" rest xs
                    match g() with
                    | End st -> st
                    | x -> failwithf "%A\n2" x
                | x -> failwithf "%A\n1" x
            Expect.equal act exp ""
        testCase "Draws return" <| fun () ->
            let p = { Name = "0"; Hand = Set.empty; Points = 0; Reason = 0 }
            // let letter = 'a', 0
            let letters = ['a'..'e'] |> List.mapi (fun i x -> x, i)

            let st = {
                PlsCircle = LZC.ofList ["0"]
                Pls = Map.empty
                Deck = letters
                Discards = []
                UsedWords = Set.empty
            }
            let n = 3
            let expLetters = List.take n letters
            let exp =
                {st with
                    Pls = Map[ p.Name, {p with Hand = Set.ofList expLetters }]
                    Deck = List.skip n st.Deck
                    Discards = []
                }
            let act =
                match draw n p st (fun st () -> End st) with
                | Draws(xs, g) ->
                    if xs <> expLetters then failwithf "exp:\n%A\nact:\n%A" expLetters xs
                    match g() with
                    | End st -> st
                    | x -> failwithf "%A\n2" x
                | x -> failwithf "%A\n1" x

            Expect.equal act exp ""
   ]
