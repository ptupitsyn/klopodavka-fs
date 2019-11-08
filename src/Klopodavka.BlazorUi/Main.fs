module Klopodavka.BlazorUi.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Klopodavka.Game

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home

/// The Elmish application's model.
type Model =
    {
        page: Page
        gameState: GameState
        error: string option
    }

let initModel =
    {
        page = Home
        gameState = Game.createGame()        
        error = None
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | NewGame
    | MakeMove of int * int
    | MakeRandomMove
    | Error of exn
    | ClearError

let update message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none

    | NewGame ->
        { model with gameState = Game.createGame() }, Cmd.none
    | MakeMove (x, y) ->
        let newState = Game.makeMove model.gameState x y
        { model with gameState = newState }, Cmd.none
    | MakeRandomMove ->
        let newState = Ai.makeRandomMove model.gameState
        { model with gameState = newState }, Cmd.none

    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let cellSize = 20

let renderTile tile avail =
    let (text, style) =
        match tile with
            | Base Red -> "🏠", "background-color: #ff9999"
            | Base Blue -> "🏠", "background-color: #80b3ff"
            | Alive Red -> "", "background-color: #ff9999"
            | Alive Blue -> "", "background-color: #80b3ff"
            | Squashed Red -> "💀", "background-color: #cc0000"
            | Squashed Blue -> "💀", "background-color: #005ce6"
            | Empty -> "", ""
            
    if (avail) then ("·", style + "; cursor: pointer") else (text, style)
    
type TableCell() =
    inherit ElmishComponent<TileInfo, Message>()
    
    override this.View cell dispatch =
        let txt, style = renderTile cell.Tile cell.Available
        td [
            attr.style style
            on.click (fun _ -> if cell.Available then (dispatch (MakeMove (cell.X, cell.Y))) else ())
        ] [
            text txt
        ]
        
    override this.ShouldRender (oldModel, newModel) =
        oldModel.Available <> newModel.Available || oldModel.Tile <> newModel.Tile

let homePage model dispatch =
    Main.Home()
        .NewGame(fun _ -> dispatch NewGame)
        .RandomMove(fun _ -> dispatch MakeRandomMove)
        .GameInfo(b [] [text (sprintf "Player: %O | Clicks: %O" model.gameState.CurrentPlayer model.gameState.ClopsLeft)])
        .GameBoard(table [] [
            forEach (Game.rows model.gameState) <| fun row ->
                tr [] [
                    forEach row <| fun cell ->
                        ecomp<TableCell, _, _> cell dispatch
                ]
        ])
        .Elt()

let view model dispatch =
    Main()
        .Body(
            cond model.page <| function
            | Home -> homePage model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
