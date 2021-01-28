module Server.Tests

open Expecto

open Shared
open Server

let all =
    testList "All"
        [
            Shared.Tests.shared
            Abstr.Tests.DrawTest
        ]

[<EntryPoint>]
let main _ = runTests defaultConfig all