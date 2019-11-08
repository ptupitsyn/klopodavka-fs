module Klopodavka.Game.Ai
open Klopodavka.Game

let makeRandomMove game =
    let board = game.Board
    let w, h = Board.size board
    
    let isNotEmpty (x, y) = (Board.tile board x y) <> Empty
    let isNotBorder (x, y) = x > 0 && y > 0 && x < w - 1 && y < h - 1
    
    let hasOneNeighbor (x, y) =
        Board.neighbors w h x y
        |> Seq.filter isNotEmpty
        |> Seq.filter (fun (x1, y1) -> x1 <> x && y1 <> y) // Diagonal
        |> Seq.length = 1
        
    let moves =
        Board.moves board game.CurrentPlayer
        |> Seq.filter isNotBorder
        |> Seq.filter hasOneNeighbor
        |> Seq.toList
        
    if moves.IsEmpty then game else
        let rnd = System.Random()
        let idx = rnd.Next moves.Length
        let (x, y) = moves.Item idx
        Game.makeMove game x y
        
let makeSquashMove game =
    let otherPlayer = Game.otherPlayer game.CurrentPlayer
    let isSquashMove (x, y) = Board.tile game.Board x y = Alive otherPlayer
    
    let moves = Board.moves game.Board game.CurrentPlayer
    let move =
        moves
        |> Seq.filter isSquashMove
        |> Seq.head
        
    let x, y = move        
    Game.makeMove game x y

let makeAiMove game =
    let tileSeq = Board.tileSeq game.Board
    let isSquashed tile = match tile with Squashed _ -> true | _ -> false
    let fightStarted = tileSeq |> Seq.exists isSquashed
        
    if fightStarted then makeSquashMove game else makeRandomMove game
