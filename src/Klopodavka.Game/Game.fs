module Klopodavka.Game.Game
open Klopodavka.Game

[<Literal>]
let DefaultClopsPerTurn = 7

let createGame(): GameState =
    {
        Board = Board.createBoard()
        CurrentPlayer = Blue
        ClopsPerTurn = DefaultClopsPerTurn
        ClopsLeft = DefaultClopsPerTurn
    }

let otherPlayer player =
    if (player = Red) then Blue else Red

let makeMove game x y =
    let nextClop game =
        let lastClop = game.ClopsLeft = 1
        let clopsLeft = if lastClop then game.ClopsPerTurn else game.ClopsLeft - 1
        let player = if lastClop then otherPlayer(game.CurrentPlayer) else game.CurrentPlayer
        { game with CurrentPlayer = player; ClopsLeft = clopsLeft }
        
    let board = Board.makeMove game.Board game.CurrentPlayer x y    
    { (nextClop game) with Board = board  }
    

let rows gameState =
    let tiles = Board.tiles gameState.Board
    let w, h = Board.size gameState.Board
    let avail = Array2D.create w h false
    
    Board.moves gameState.Board gameState.CurrentPlayer
        |> Seq.iter (fun (x, y) -> avail.[x, y] <- true)
        
    seq {
        for y = 0 to h - 1 do yield seq {
            for x = 0 to w - 1 do yield { X = x; Y = y; Tile = tiles.[x, y]; Available = avail.[x, y] }
        }
    } 

let makeRandomMove gameState =
    let tiles = Board.tiles gameState.Board
    let w, h = Board.size gameState.Board
    
    let isNotEmpty (x, y) = tiles.[x, y] <> Empty
    
    let hasOneNeighbor (x, y) =
        Board.neighbors w h x y
        |> Seq.filter isNotEmpty
        |> Seq.filter (fun (x1, y1) -> x1 <> x && y1 <> y) // Diagonal
        |> Seq.length = 1
        
    let moves =
        Board.moves gameState.Board gameState.CurrentPlayer
        |> Seq.filter hasOneNeighbor
        |> Seq.toList
        
    if moves.IsEmpty then gameState else
        let rnd = System.Random()
        let idx = rnd.Next moves.Length
        let (x, y) = moves.Item idx
        makeMove gameState x y
