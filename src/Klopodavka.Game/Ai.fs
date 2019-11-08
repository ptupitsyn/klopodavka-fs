module Klopodavka.Game.Ai

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
        Game.makeMove gameState x y
