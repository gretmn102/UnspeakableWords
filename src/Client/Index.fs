module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type State =
    {
        MessageSendResult: string Deferred
    }

type Msg =
    | SendMessage
    | SendMessageResult of string

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IApi>

let init(): State * Cmd<Msg> =
    let state =
        {
            MessageSendResult = HasNotStartedYet
        }
    state, Cmd.none


let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | SendMessage ->
        let cmd = Cmd.OfAsync.perform todosApi.sendMessage () SendMessageResult
        let state = { state with MessageSendResult = InProgress }
        state, cmd
    | SendMessageResult res ->
        { state with MessageSendResult = Resolved res }, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let navBrand =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
    ]

let containerBox (state : State) (dispatch : Msg -> unit) =
    Box.box' [ ] [
        Button.a [
            Button.Color IsPrimary
            // Button.Disabled (Todo.isValid state.Input |> not)
            Button.OnClick (fun _ -> dispatch SendMessage)
        ] [
            str "Send message"
        ]

        match state.MessageSendResult with
        | Resolved x ->
            div [] [str (sprintf "%A" x)]
        | InProgress ->
            div [] [
                Fa.i [ Fa.IconOption.Size Fa.ISize.Fa3x; Fa.Solid.Spinner; Fa.Spin ] []
            ]
        | HasNotStartedYet -> ()
    ]

let view (state : State) (dispatch : Msg -> unit) =
    Hero.hero [
        // Hero.Color IsPrimary
        Hero.IsFullHeight
        Hero.Props [
            Style [
                // Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                BackgroundSize "cover"
            ]
        ]
    ] [
        Hero.head [ ] [
            Navbar.navbar [ ] [
                Container.container [ ] [ navBrand ]
            ]
        ]

        Hero.body [ ] [
            Container.container [ ] [
                Column.column [
                    Column.Width (Screen.All, Column.Is6)
                    Column.Offset (Screen.All, Column.Is3)
                ] [
                    Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "UnspeakableWords" ]
                    containerBox state dispatch
                ]
            ]
        ]
    ]
