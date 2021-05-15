module Abstr.Tests
open Fuchu

open FsharpMyExtension
#if INTERACTIVE
#load @"..\..\src\Shared\Shared.fs"
#load @"..\..\src\Server\Abstr.fs"
#endif
open Shared
open Abstr
open FsharpMyExtension.ListZipperCircle2

let equal act exp msg = Assert.Equal(msg, exp, act)

[<Tests>]
let DrawTest =
    testList "DrawTest" [
        testCase "DrawsWithDiscardReuse 1 case" <| fun () ->
            let p = { Name = "0"; Hand = Set.empty; Points = 0; SanityPoints = 0 }
            let letter = 0
            let st = {
                PlsCircle = LZC.ofList [p.Name]
                Pls = Map.empty
                Deck = []
                Discards = [letter]
                UsedWords = Set.empty
                UsedWordsBy = []
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
            equal act exp ""
        testCase "DrawsWithDiscardReuse 2 return" <| fun () ->
            let p = { Name = "0"; Hand = Set.empty; Points = 0; SanityPoints = 0 }
            let letters = [1..7] |> List.mapi (fun i x -> i)

            let st = {
                PlsCircle = LZC.ofList [p.Name]
                Pls = Map.empty
                Deck = List.take 3 letters
                Discards = List.skip 3 letters
                UsedWords = Set.empty
                UsedWordsBy = []
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
            equal act exp ""
        testCase "Draws return" <| fun () ->
            let p = { Name = "0"; Hand = Set.empty; Points = 0; SanityPoints = 0 }
            let letters = [0..4] |> List.mapi (fun i x -> x)

            let st = {
                PlsCircle = LZC.ofList [p.Name]
                Pls = Map.empty
                Deck = letters
                Discards = []
                UsedWords = Set.empty
                UsedWordsBy = []
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

            equal act exp ""
        testCase "draw MoveWithoutDraw" <| fun () ->
            let p = { Name = "p1"; Hand = Set [0]; Points = 0; SanityPoints = 0 }
            let pls =
                [ p.Name, p ]
            let st = {
                PlsCircle = LZC.ofList (List.map fst pls)
                Pls = Map.ofList pls
                Deck = []
                Discards = []
                UsedWords = Set.empty
                UsedWordsBy = []
            }
            let act =
                match draw 2 p st (fun st () -> End st) with
                | MoveWithoutDraw f ->
                    match f () with
                    | End(x) ->
                        x
                    | _ -> failwith ""
                | x -> failwithf "act: MoveWithoutDraw\nexp:%A" x
            // act = st
            equal act st ""
        testCase "draw DeckIsOver" <| fun () ->
            let p = { Name = "p1"; Hand = Set []; Points = 0; SanityPoints = 0 }
            let pls =
                [ p.Name, p ]
            let letters = [0; 1]
            let st = {
                PlsCircle = LZC.ofList (List.map fst pls)
                Pls = Map.ofList pls
                Deck = letters
                Discards = []
                UsedWords = Set.empty
                UsedWordsBy = []
            }
            let act =
                match draw 2 p st (fun st () -> End st) with
                | Draws(xs, f) when xs = letters ->
                    match f () with
                    | End(x) ->
                        x
                    | _ -> failwith ""
                | x -> failwithf "exp: Draws\nact:%A" x
            let exp =
                { st with
                    Deck = []
                    Pls =
                        let p =
                            let p = st.Pls.["p1"]
                            { p with Hand = Set.ofList letters }
                        Map.add p.Name p st.Pls
                }
            equal act exp ""
        testCase "draw RemovePlayerBecauseCardsIsLeft"  <| fun () ->
            let p1 = { Name = "p1"; Hand = Set []; Points = 0; SanityPoints = 0 }
            let p2 = { Name = "p2"; Hand = Set [0]; Points = 0; SanityPoints = 0 }
            let p3 = { Name = "p3"; Hand = Set [0]; Points = 0; SanityPoints = 0 }
            let pls =
                [
                    p1.Name, p1
                    p2.Name, p2
                    p3.Name, p3
                ]

            let st = {
                PlsCircle = LZC.ofList (List.map fst pls)
                Pls = Map.ofList pls
                Deck = []
                Discards = []
                UsedWords = Set.empty
                UsedWordsBy = []
            }
            let exp =
                match draw 2 p1 st (fun st () -> End st) with
                | RemovePlayerBecauseCardsIsLeft f ->
                    match f () with
                    | End st ->
                        st
                    | _ -> failwith ""
                | x -> failwithf "exp: Draws\nact:%A" x
            let act =
                { st with
                    PlsCircle = LZC.removeL st.PlsCircle |> Option.get
                }
            // act = exp
            equal act exp ""

   ]

[<Tests>]
let sanityCheckTests =
    testList "sanityCheckTests" [
        testCase "sanityCheck Pass" <| fun () ->
            let pls =
                [
                    "p1", { Name = "p1"; Hand = Set [0..2]; Points = 0; SanityPoints = 0 }
                    "p2", { Name = "p2"; Hand = Set [3..5]; Points = 0; SanityPoints = 0 }
                    "p3", { Name = "p3"; Hand = Set [6..8]; Points = 0; SanityPoints = 0 }
                ]
            let st = {
                PlsCircle = LZC.ofList (List.map fst pls)
                Pls = Map.ofList pls
                Deck = []
                Discards = []
                UsedWords = Set.empty
                UsedWordsBy = []
            }

            let x = sanityCheck 1 10 [0..2] st (fun st () -> End st)
            let act =
                match x.SanityCheckResult with
                | Pass(RemovePlayerBecauseCardsIsLeft f) ->

                    match f () with
                    | End(x) ->
                        x
                    | _ -> failwith ""
                | x -> failwithf "exp: RemovePlayerBecauseCardsIsLeft\nact:\n%A" x
            ()
        testCase "sanityCheck GotInsane" <| fun () ->
            let p1 = { Name = "p1"; Hand = Set [0..2]; Points = 0; SanityPoints = 1 }
            let p2 = { Name = "p2"; Hand = Set [3..5]; Points = 0; SanityPoints = 1 }
            let p3 = { Name = "p3"; Hand = Set [6..8]; Points = 0; SanityPoints = 1 }
            let pls =
                [
                    p1.Name, p1
                    p2.Name, p2
                    p3.Name, p3
                ]

            let st = {
                PlsCircle = LZC.ofList (List.map fst pls)
                Pls = Map.ofList pls
                Deck = []
                Discards = []
                UsedWords = Set.empty
                UsedWordsBy = []
            }

            let x = sanityCheck 2 1 [0] st (fun st () -> End st)
            let act =
                match x.SanityCheckResult with
                | NotPass (GotInsane (letters, f)) when letters = Set.toList p1.Hand ->
                    match f () with
                    | End(x) ->
                        x
                    | _ -> failwith ""
                | x -> failwithf "exp: NotPass (GotInsane (letters, f))\nact:\n%A" x
            let exp =
                { st with
                    PlsCircle = LZC.removeL st.PlsCircle |> Option.get
                    Pls =
                        let p1 = { p1 with Hand = Set.empty; SanityPoints = 0; Points = 2 }
                        Map.add p1.Name p1 st.Pls
                    Discards = Set.toList p1.Hand
                }
            equal act exp ""
    ]
