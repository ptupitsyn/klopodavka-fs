namespace Klopodavka.Game

type Player = Red | Blue

type Tile =
    | Empty
    | Base of player: Player
    | Alive of player: Player
    | Squashed of player: Player

type Tiles = Tiles of Tile[,]

type GameState = {
    Board: Tiles;
    CurrentPlayer: Player;
    ClopsPerTurn: int32;
    ClopsLeft: int32;
}

type TileInfo = {
    X: int32;
    Y: int32;
    Tile: Tile;
    Available: bool;
}
    