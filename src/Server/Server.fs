module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

// open FsharpMyExtension
// open FsharpMyExtension.Either

open Prelude
let iApi:IApi =
    {
        login = fun userId ->
            async { return m.PostAndReply(fun r -> Login(userId, r)) }
        getState = fun userId ->
            async { return m.PostAndReply(fun r -> GetState(userId, r)) }
        move = fun x ->
            async { return m.PostAndReply(fun r -> Move(x, r)) }
        getSet = fun () ->
            async { return Set [ 1 ] }
    }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue iApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8086"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
