module IOUtils
 
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
