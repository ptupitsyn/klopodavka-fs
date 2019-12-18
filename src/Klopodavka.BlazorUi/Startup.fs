namespace Klopodavka.BlazorUi.Client

open Microsoft.AspNetCore.Blazor.Hosting
open Microsoft.AspNetCore.Components.Builder

type Startup() =

    member __.Configure(app: IComponentsApplicationBuilder) =
        app.AddComponent<Main.MyApp>("#main")

module Program =

    [<EntryPoint>]
    let Main _ =
        BlazorWebAssemblyHost.CreateDefaultBuilder()
            .UseBlazorStartup<Startup>()
            .Build()
            .Run()
        0
