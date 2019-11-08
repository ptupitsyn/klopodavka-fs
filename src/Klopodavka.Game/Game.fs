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
    let board = Board.makeMove game.Board game.CurrentPlayer x y
    let lastClop = game.ClopsLeft = 1
    let clopsLeft = if lastClop then game.ClopsPerTurn else game.ClopsLeft - 1
    let player = if lastClop then otherPlayer(game.CurrentPlayer) else game.CurrentPlayer
    { game with CurrentPlayer = player; ClopsLeft = clopsLeft; Board = board }
    
let tilesAndSize game =
    let tiles = Board.tiles game.Board
    let w, h = Board.size game.Board
    (tiles, w, h)    

let rows game =
    let tiles, w, h = tilesAndSize game
    let avail = Array2D.create w h false
    
    Board.moves game.Board game.CurrentPlayer
        |> Seq.iter (fun (x, y) -> avail.[x, y] <- true)
        
    seq {
        for y = 0 to h - 1 do yield seq {
            for x = 0 to w - 1 do yield { X = x; Y = y; Tile = tiles.[x, y]; Available = avail.[x, y] }
        }
    }