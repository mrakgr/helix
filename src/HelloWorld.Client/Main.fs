module HelloWorld.Client.Main

open System
open System.Collections.Generic
open Blazor.Diagrams.Components
open Blazor.Diagrams.Core
open Blazor.Diagrams.Core.Geometry
open Blazor.Diagrams.Core.Models
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.AspNetCore.Components
open MudBlazor

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/counter">] Counter
    | [<EndPoint "/data">] Data
    | [<EndPoint "/DiagramTrial">] DiagramTrial

/// The Elmish application's model.
type Model =
    {
        page: Page
        counter: int
        books: Book[] option
        error: string option
        username: string
        password: string
        signedInAs: option<string>
        signInFailed: bool
    }

and Book =
    {
        title: string
        author: string
        publishDate: DateTime
        isbn: string
    }

let initModel =
    {
        page = Home
        counter = 0
        books = None
        error = None
        username = ""
        password = ""
        signedInAs = None
        signInFailed = false
    }

/// Remote service definition.
type BookService =
    {
        /// Get the list of all books in the collection.
        getBooks: unit -> Async<Book[]>

        /// Add a book in the collection.
        addBook: Book -> Async<unit>

        /// Remove a book from the collection, identified by its ISBN.
        removeBookByIsbn: string -> Async<unit>

        /// Sign into the application.
        signIn : string * string -> Async<option<string>>

        /// Get the user's name, or None if they are not authenticated.
        getUsername : unit -> Async<string>

        /// Sign out from the application.
        signOut : unit -> Async<unit>
    }

    interface IRemoteService with
        member this.BasePath = "/books"

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | Increment
    | Decrement
    | SetCounter of int
    | GetBooks
    | GotBooks of Book[]
    | SetUsername of string
    | SetPassword of string
    | GetSignedInAs
    | RecvSignedInAs of option<string>
    | SendSignIn
    | RecvSignIn of option<string>
    | SendSignOut
    | RecvSignOut
    | Error of exn
    | ClearError

let update remote message model =
    let onSignIn = function
        | Some _ -> Cmd.ofMsg GetBooks
        | None -> Cmd.none
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none

    | Increment ->
        { model with counter = model.counter + 1 }, Cmd.none
    | Decrement ->
        { model with counter = model.counter - 1 }, Cmd.none
    | SetCounter value ->
        { model with counter = value }, Cmd.none

    | GetBooks ->
        let cmd = Cmd.OfAsync.either remote.getBooks () GotBooks Error
        { model with books = None }, cmd
    | GotBooks books ->
        { model with books = Some books }, Cmd.none

    | SetUsername s ->
        { model with username = s }, Cmd.none
    | SetPassword s ->
        { model with password = s }, Cmd.none
    | GetSignedInAs ->
        model, Cmd.OfAuthorized.either remote.getUsername () RecvSignedInAs Error
    | RecvSignedInAs username ->
        { model with signedInAs = username }, onSignIn username
    | SendSignIn ->
        model, Cmd.OfAsync.either remote.signIn (model.username, model.password) RecvSignIn Error
    | RecvSignIn username ->
        { model with signedInAs = username; signInFailed = Option.isNone username }, onSignIn username
    | SendSignOut ->
        model, Cmd.OfAsync.either remote.signOut () (fun () -> RecvSignOut) Error
    | RecvSignOut ->
        { model with signedInAs = None; signInFailed = false }, Cmd.none

    | Error RemoteUnauthorizedException ->
        { model with error = Some "You have been logged out."; signedInAs = None }, Cmd.none
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html"> // What this does is use a type provider to analyze the html file and derive the types from that.

let homePage model dispatch =
    Main.Home().Elt()

let counterPage model dispatch =
    Main.Counter()
        .Decrement(fun _ -> dispatch Decrement)
        .Increment(fun _ -> dispatch Increment)
        .Value(model.counter, fun v -> dispatch (SetCounter v))
        .Elt() // I am not sure what this Elt is here.

let dataPage model (username: string) dispatch =
    Main
        .Data()
        .Reload(fun _ -> dispatch GetBooks)
        .Username(username)
        .SignOut(fun _ -> dispatch SendSignOut)
        .Rows(cond model.books <| function
            | None ->
                Main.EmptyData().Elt()
            | Some books ->
                forEach books <| fun book ->
                    tr {
                        td { book.title }
                        td { book.author }
                        td { book.publishDate.ToString("yyyy-MM-dd") }
                        td { book.isbn }
                    })
        .Elt()

let signInPage model dispatch =
    Main.SignIn()
        .Username(model.username, fun s -> dispatch (SetUsername s))
        .Password(model.password, fun s -> dispatch (SetPassword s))
        .SignIn(fun _ -> dispatch SendSignIn)
        .ErrorNotification(
            cond model.signInFailed <| function
            | false -> empty()
            | true ->
                Main.ErrorNotification()
                    .HideClass("is-hidden")
                    .Text("Sign in failed. Use any username and the password \"password\".")
                    .Elt()
        )
        .Elt()

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()
        
type HelixDiagram() =
    inherit Component()
    
    let diagram =
        let options = DiagramOptions(
            DeleteKey = "Delete", // What key deletes the selected nodes/links
            DefaultNodeComponent = null, // Default component for nodes
            AllowMultiSelection = true, // Whether to allow multi selection using CTRL
            Links = DiagramLinkOptions(), // Options related to links
            Zoom = DiagramZoomOptions(
                Minimum = 0.5, // Minimum zoom value
                Inverse = true // Whether to inverse the direction of the zoom when using the wheel
                ) // Other
        )
        Diagram(options)
        
    let new_node (x,y) =
        let node = NodeModel(Point(x, y))
        node.AddPort(PortAlignment.Bottom) |> ignore
        node.AddPort(PortAlignment.Top) |> ignore
        node.AddPort(PortAlignment.Left) |> ignore
        node.AddPort(PortAlignment.Right) |> ignore
        node
    
    do
        let node1 = new_node(50, 50)
        let node2 = new_node(300, 300)
        let node3 = new_node(300, 50)
        diagram.Nodes.Add(seq { node1; node2; node3})
        diagram.Links.Add(LinkModel(node1.GetPort(PortAlignment.Right), node2.GetPort(PortAlignment.Left)))
    
    override this.Render() =
        let digram_canvas = comp<CascadingValue<Diagram>> {
            "Value" => diagram
            comp<DiagramCanvas>
            }
        comp<MudMenu> {
            // "PositionAtCursor" => true
            // "ActivationEvent" => MouseEvent.RightClick
            attr.fragment "ActivatorContent" (
                comp<MudButton> {
                    "Hello"
                }
                )
            attr.fragment "ChildContent" (concat {
                comp<MudMenuItem> { div {"Text"} }
                comp<MudMenuItem> { div {"Compilation"} }
            })
        }
        
        
let diagramTrialPage model dispatch =
    Main.DiagramTrial() 
        .DiagramBody(comp<HelixDiagram> {attr.empty()})
        .Elt()

let view model dispatch =
    comp<MudMenu> {
        // "PositionAtCursor" => true
        // "ActivationEvent" => MouseEvent.RightClick
        attr.fragment "ActivatorContent" (comp<MudButton> {
            on.click (fun e -> printfn "Hello Clicked")
            "Hello"
        })
        attr.fragment "ChildContent" (div {
            comp<MudMenuItem> { div {"Text"} }
            comp<MudMenuItem> { div {"Compilation"} }
        })
    }
    // Main()
    //     .Menu(concat {
    //         menuItem model Home "Home"
    //         menuItem model Counter "Counter"
    //         menuItem model Data "Download data"
    //         menuItem model DiagramTrial "Diagram Trial"
    //     })
    //     .Body(
    //         cond model.page <| function
    //         | Home -> homePage model dispatch
    //         | Counter -> counterPage model dispatch
    //         | DiagramTrial -> diagramTrialPage model dispatch
    //         | Data ->
    //             cond model.signedInAs <| function
    //             | Some username -> dataPage model username dispatch
    //             | None -> signInPage model dispatch
    //     )
    //     .Error(
    //         cond model.error <| function
    //         | None -> empty()
    //         | Some err ->
    //             Main.ErrorNotification()
    //                 .Text(err)
    //                 .Hide(fun _ -> dispatch ClearError)
    //                 .Elt()
    //     )
    //     .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let bookService = this.Remote<BookService>()
        let update = update bookService
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg GetSignedInAs) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
