module Klopodavka.Game.Ai

let makeRandomMove game =
    let tiles, w, h = Game.tilesAndSize game
    
    let isNotEmpty (x, y) = tiles.[x, y] <> Empty
    let isNotBorder (x, y) = x > 0 && y > 0 && x < w - 1 && y < h - 1
    
    let hasOneNeighbor (x, y) =
        Board.neighbors w h x y
        |> Seq.filter isNotEmpty
        |> Seq.filter isNotBorder
        |> Seq.filter (fun (x1, y1) -> x1 <> x && y1 <> y) // Diagonal
        |> Seq.length = 1
        
    let moves =
        Board.moves game.Board game.CurrentPlayer
        |> Seq.filter hasOneNeighbor
        |> Seq.toList
        
    if moves.IsEmpty then game else
        let rnd = System.Random()
        let idx = rnd.Next moves.Length
        let (x, y) = moves.Item idx
        Game.makeMove game x y

let makeAiMove game =
    let tiles, w, h = Game.tilesAndSize game
    game
