open Fuchu
open FsharpMyExtension
open FsharpMyExtension.FSharpExt

[<Tests>]
let simpleTest = 
    testCase "A simple test" <| 
        fun _ ->
            //Assert.Equal("2+3", 4, 2+3)
            (10, "20") |> function (a, b) as x -> Assert.Equal("comma a b = (a, b)", x, comma 10 b)
let ran = System.Random() |> fun x k n-> x.Next(k, n)

[<EntryPoint>]
let main arg =
    defaultMainThisAssembly arg
