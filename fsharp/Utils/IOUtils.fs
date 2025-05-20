namespace Utils
module IOUtils = 

    open System

    let tryRead () = 
        try
            Ok (Console.ReadLine())
        with
        | :? IO.IOException -> Error "Error reading input."
        | :? OutOfMemoryException -> Error "Out of memory."
        | :? ArgumentOutOfRangeException -> Error "Argument out of range."

    let tryReadInt () =
        match tryRead () with
        | Ok s ->
            try
                Ok (int s)      
            with
            | :? FormatException -> Error ("Invalid input.", s)
        | Error errorValue -> failwith errorValue


    let display2DArray (array: string[,]) (previousArray: string[,] option) =
        //Console.Clear()
        let isCodePointWide codePoint =
            // Check if the Unicode code point belongs to a wide character range
            codePoint >= 0x1100 && codePoint <= 0x115F ||
            codePoint >= 0x2329 && codePoint <= 0x232A ||
            codePoint >= 0x2E80 && codePoint <= 0x303E ||
            codePoint >= 0x3040 && codePoint <= 0xA4CF ||
            codePoint >= 0xAC00 && codePoint <= 0xD7A3 ||
            codePoint >= 0xF900 && codePoint <= 0xFAFF ||
            codePoint >= 0xFE10 && codePoint <= 0xFE19 ||
            codePoint >= 0xFE30 && codePoint <= 0xFE6F ||
            codePoint >= 0xFF00 && codePoint <= 0xFF60 ||
            codePoint >= 0xFFE0 && codePoint <= 0xFFE6 ||
            codePoint >= 0x1F300 && codePoint <= 0x1F5FF ||
            codePoint >= 0x1F600 && codePoint <= 0x1F64F ||
            codePoint >= 0x1F680 && codePoint <= 0x1F6FF ||
            codePoint >= 0x2600 && codePoint <= 0x26FF ||
            codePoint >= 0x2700 && codePoint <= 0x27BF ||
            codePoint >= 0xFE00 && codePoint <= 0xFE0F ||
            codePoint >= 0x1F900 && codePoint <= 0x1F9FF

        let getDisplayWidth (s: string) =
            if String.IsNullOrEmpty s then 0 else
            let si = System.Globalization.StringInfo(s)
            [| for i in 0 .. si.LengthInTextElements - 1 ->
                let te = si.SubstringByTextElements(i, 1)
                System.Char.ConvertToUtf32(te, 0) |]
            |> Array.sumBy (fun cp -> if isCodePointWide cp then 2 else 1)

        let rows = array.GetLength 0
        let cols = array.GetLength 1
        let displayRows = rows * 2 + 1
        let displayCols = cols * 2 + 1
        let maxLength =
            array
            |> Array2D.map getDisplayWidth
            |> Seq.cast<int>
            |> Seq.max


        let stringBuilder = Text.StringBuilder()

        let displayLine (evenCharacter: char) (firstCharacter: char)  (oddCharacter: char) (lastCharacter: char) =
            stringBuilder.Append firstCharacter |> ignore
            for j in 0 .. displayCols - 3 do
                if j % 2 = 0 then
                    for _ in 0 .. maxLength - 1 do
                        stringBuilder.Append evenCharacter |> ignore
                else
                    stringBuilder.Append oddCharacter |> ignore
            stringBuilder.Append lastCharacter |> ignore
            
        let displayLine = displayLine '─'
        for i in 0 .. displayRows - 1 do
            if i = 0 then
                displayLine '┌'  '┬' '┐'
            else if i = displayRows - 1 then
                displayLine '└' '┴' '┘'
            else if i % 2 = 0 then
                displayLine '├' '┼' '┤'
            else 
                for j in 0 .. displayCols - 1 do
                    if j % 2 = 0 then
                        stringBuilder.Append '│' |> ignore
                    else
                        let row = i / 2
                        let col = j / 2
                        let value = array.[col, row]
                        let spaceFill = maxLength - getDisplayWidth value

                        for _ in 0 .. spaceFill / 2 - 1 do
                            stringBuilder.Append ' ' |> ignore

                        stringBuilder.Append $"{value}" |> ignore

                        for _ in 0 .. spaceFill / 2 - 1 + spaceFill % 2 do
                            stringBuilder.Append ' ' |> ignore


            stringBuilder.Append '\n' |> ignore

        stringBuilder.ToString()


    let display2DListOfFloats (list: float list list) =

        let getDisplayWidth (s: float) =
            s.ToString().Length

        let rows = list.Length
        let cols = list.[0].Length
        let displayRows = rows * 2 + 1
        let displayCols = cols * 2 + 1
        let maxLength =
            list
            |> List.collect (fun row -> row |> List.map getDisplayWidth)
            |> List.max


        let stringBuilder = Text.StringBuilder()

        let displayLine (evenCharacter: char) (firstCharacter: char)  (oddCharacter: char) (lastCharacter: char) =
            stringBuilder.Append firstCharacter |> ignore
            for j in 0 .. displayCols - 3 do
                if j % 2 = 0 then
                    for _ in 0 .. maxLength - 1 do
                        stringBuilder.Append evenCharacter |> ignore
                else
                    stringBuilder.Append oddCharacter |> ignore
            stringBuilder.Append lastCharacter |> ignore
            
        let displayLine = displayLine '─'
        for i in 0 .. displayRows - 1 do
            if i = 0 then
                displayLine '┌'  '┬' '┐'
            else if i = displayRows - 1 then
                displayLine '└' '┴' '┘'
            else if i % 2 = 0 then
                displayLine '├' '┼' '┤'
            else 
                for j in 0 .. displayCols - 1 do
                    if j % 2 = 0 then
                        stringBuilder.Append '│' |> ignore
                    else
                        let row = i / 2
                        let col = j / 2
                        let value = list.[row].[col]
                        let spaceFill = maxLength - getDisplayWidth value

                        for _ in 0 .. spaceFill / 2 - 1 do
                            stringBuilder.Append ' ' |> ignore

                        stringBuilder.Append $"{value}" |> ignore

                        for _ in 0 .. spaceFill / 2 - 1 + spaceFill % 2 do
                            stringBuilder.Append ' ' |> ignore


            stringBuilder.Append '\n' |> ignore

        stringBuilder.ToString()