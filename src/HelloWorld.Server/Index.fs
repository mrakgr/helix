module HelloWorld.Server.Index

open Bolero
open Bolero.Html
open Bolero.Server.Html
open HelloWorld
open MudBlazor

let page = doctypeHtml {
    head {
        meta { attr.charset "UTF-8" }
        meta { attr.name "viewport"; attr.content "width=device-width, initial-scale=1.0" }
        title { "Bolero Application" }
        ``base`` { attr.href "/" }
        link { attr.rel "stylesheet"; attr.href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css" }
        // link { attr.rel "stylesheet"; attr.href "HelloWorld.Client.styles.css" }
        link { attr.rel "stylesheet"; attr.href "css/index.css" }
        link { attr.rel "stylesheet"; attr.href "_content/Z.Blazor.Diagrams/style.min.css" }
        link { attr.rel "stylesheet"; attr.href "_content/Z.Blazor.Diagrams/default.styles.min.css" }
        link { attr.rel "stylesheet"; attr.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap" }
        link { attr.rel "stylesheet"; attr.href "_content/MudBlazor/MudBlazor.min.css" }
    }
    body {
        nav {
            attr.``class`` "navbar is-dark"
            "role" => "navigation"
            attr.aria "label" "main navigation"
            div {
                attr.``class`` "navbar-brand"
                a {
                    attr.``class`` "navbar-item has-text-weight-bold is-size-5"
                    attr.href "https://fsbolero.io"
                    img { attr.style "height:40px"; attr.src "https://github.com/fsbolero/website/raw/master/src/Website/img/wasm-fsharp.png" }
                    "  Bolero"
                }
            }
        }
        
        // Yeah, these 3 need to be here.
        comp<MudThemeProvider>
        comp<MudDialogProvider>
        comp<MudSnackbarProvider>
        
        comp<MudLayout> {
            comp<MudMainContent> {
                div {
                    attr.id "main"
                    comp<Client.Main.MyApp>
                }
            }
        }
        
        boleroScript
        script { attr.src "_content/Z.Blazor.Diagrams/script.min.js" }
        script { attr.src "_content/MudBlazor/MudBlazor.min.js" }
    }
}
