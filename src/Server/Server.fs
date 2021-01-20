module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

// open FsharpMyExtension
// open FsharpMyExtension.Either


let counter = ref 0

let sendMessage () =
    let res = sprintf "counter: %d" !counter
    counter := !counter + 1
    res

let todosApi =
    {
        sendMessage = fun x ->
            async { return sendMessage () }
    }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue todosApi
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
