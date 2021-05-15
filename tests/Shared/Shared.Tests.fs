module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Fuchu
#endif

open Shared
#if FABLE_COMPILER
open Fable.Mocha
#else
[<Tests>]
#endif
let shared =
    testList "Shared" [
    ]