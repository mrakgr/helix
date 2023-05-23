namespace HelloWorld.Client

open Microsoft.AspNetCore.Components.WebAssembly.Hosting
open Bolero.Remoting.Client
open MudBlazor.Services

module Program =

    [<EntryPoint>]
    let Main args =
        let builder = WebAssemblyHostBuilder.CreateDefault(args)
        builder.RootComponents.Add<Main.MyApp>("#main")
        builder.Services
            .AddMudServices() // These services need to be in both projects, but the context menu still isn't working.
            .AddBoleroRemoting(builder.HostEnvironment) |> ignore
        builder.Build().RunAsync() |> ignore
        0
