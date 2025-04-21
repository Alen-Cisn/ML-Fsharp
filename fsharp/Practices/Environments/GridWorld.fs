namespace Practices.Environments

open GridBroad
open System

type Mode = Static | Player | Random

type Action = MoveLeft | MoveRight | MoveUp | MoveDown | NoOp

type Gridworld(size: int, mode: Mode) as self =
    let board =
        if size >= 4 then GridBoard size
        else
            Console.WriteLine "Minimum board size is 4. Initialized to size 4."
            GridBoard 4

    do
        // Initialize board components
        board.AddPiece("Player", " 🯅𐃉 ", (0,0))
        board.AddPiece("Goal", "🥅", (1,0))
        board.AddPiece("Pit", "🔥", (2,0))
        board.AddPiece("Wall", "🧱", (3,0))

        match mode with
        | Static -> self.InitGridStatic()
        | Player -> self.InitGridPlayer()
        | Random -> self.InitGridRandom()

    member private this.InitGridStatic() =
        board.MovePiece("Player", (0, 3))
        board.MovePiece("Goal", (0, 0))
        board.MovePiece("Pit", (0, 1))
        board.MovePiece("Wall", (1, 1))

    member private this.InitGridPlayer() =
        this.InitGridStatic()
        let rec placePlayer() =
            board.MovePiece("Player", randPair 0 board.Size)
            if not (this.ValidateBoard()) then placePlayer()
        placePlayer()

    member private this.InitGridRandom() =
        let rec randomize() =
            board.MovePiece("Player", randPair 0 board.Size)
            board.MovePiece("Goal", randPair 0 board.Size)
            board.MovePiece("Pit", randPair 0 board.Size)
            board.MovePiece("Wall", randPair 0 board.Size)
            if not (this.ValidateBoard()) then randomize()
        randomize()

    member private this.ValidateBoard() =
        let components = board.Components
        let positions = [
            components.["Player"].Pos
            components.["Goal"].Pos
            components.["Pit"].Pos
            components.["Wall"].Pos
        ]

        if positions.Length <> (positions |> List.distinct).Length then false
        else
            let size = board.Size - 1
            let corners = [ 0,0; 0,size; size,0; size,size ]
            let checkCornerMoves pos =
                [ 0,1; 1,0; -1,0; 0,-1 ]
                |> List.exists (fun move ->
                    this.ValidateMoveAtPos(pos, move) <= 1)

            let playerInCorner = List.contains components.["Player"].Pos corners
            let goalInCorner = List.contains components.["Goal"].Pos corners
            
            (not playerInCorner || checkCornerMoves components.["Player"].Pos) &&
            (not goalInCorner || checkCornerMoves components.["Goal"].Pos)

    member private _.ValidateMoveAtPos(pos, (dx, dy)) =
        let newPos = addTuple pos (dx, dy)
        let maxCoord = board.Size - 1
        
        let components = board.Components
        if newPos = components.["Wall"].Pos then 1
        elif fst newPos < 0 || snd newPos < 0 then 1
        elif fst newPos > maxCoord || snd newPos > maxCoord then 1
        elif newPos = components.["Pit"].Pos then 2
        else 0

    member private this.ValidateMove(pieceName, (dx, dy)) =
        this.ValidateMoveAtPos(board.Components.[pieceName].Pos, (dx, dy))

    member this.MakeMove action =
        let delta = match action with
                    | MoveUp ->  0, -1
                    | MoveDown -> 0, 1
                    | MoveLeft -> -1, 0
                    | MoveRight -> 1, 0
                    | _ -> 0, 0
        
        match this.ValidateMove("Player", delta) with
        | 0 | 2 ->
            let newPos = addTuple board.Components.["Player"].Pos delta
            board.MovePiece("Player", newPos)
        | _ -> ()

    member _.Reward() =
        let pc = board.Components.["Player"].Pos
        if pc = board.Components.["Pit"].Pos then -10
        elif pc = board.Components.["Goal"].Pos then 10
        else -1

    member _.Display() = board.Render()