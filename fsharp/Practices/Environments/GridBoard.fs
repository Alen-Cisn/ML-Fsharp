namespace Practices.Environments
module GridBroad =
    open System
    open System.Collections.Generic

    type BoardPiece<'a> = {
        Name : 'a
        Code : string
        Pos : int * int
    }

    type BoardMask<'a> = {
        Name : 'a
        Mask : bool[,]
        Code : string
    } with
        member this.GetPositions() =
            [ for x in 0..this.Mask.GetLength 0-1 do
                for y in 0..this.Mask.GetLength 1-1 do
                    if this.Mask.[x,y] then yield x,y ]

    type GridBoard<'a when 'a: equality>(size: int)=
        let components = Dictionary<'a, BoardPiece<'a>>()
        let masks = Dictionary<'a, BoardMask<'a>>()
        
        member _.Size = size
        
        member _.Components = components

        member _.AddPiece(name, code, ?pos) =
            let position = defaultArg pos (0, 0)
            components.[name] <- { Name = name; Code = code; Pos = position }
        
        member _.AddMask(name, mask, code) =
            masks.[name] <- { Name = name; Mask = mask; Code = code }
        
        member _.MovePiece(name, pos) =
            let isBlocked =
                masks.Values
                |> Seq.exists (fun m -> m.GetPositions() |> List.contains pos)
            
            if not isBlocked && components.ContainsKey name then
                components.[name] <- { components.[name] with Pos = pos }
        
        member _.DelPiece name = components.Remove name |> ignore
        
        member _.Render() =
            let board = Array2D.init size size (fun _ _ -> " ")
            components.Values |> Seq.iter (fun p ->
                let x,y = p.Pos
                board.[x,y] <- p.Code)
            masks.Values |> Seq.iter (fun m ->
                m.GetPositions() |> List.iter (fun (x,y) ->
                    board.[x,y] <- m.Code))
            board
        
        member _.RenderNp() =
            let layers = components.Count + masks.Count
            let board = Array3D.zeroCreate layers size size
            let mutable layer = 0
            
            components.Values |> Seq.iter (fun p ->
                let x,y = p.Pos
                board.[layer,x,y] <- 1uy
                layer <- layer + 1)
            
            masks.Values |> Seq.iter (fun m ->
                m.GetPositions() |> List.iter (fun (x,y) ->
                    board.[layer,x,y] <- 1uy)
                layer <- layer + 1)
            
            board

    let private rnd = Random 3

    let randPair s e =
        rnd.Next(s, e), rnd.Next(s, e)

    let addTuple (x1, y1) (x2, y2) = 
        x1 + x2, y1 + y2