namespace Practices.Math

module Matrices =

    open System
    open Utils.IOUtils

    // Método de eliminación de Gauss
    let métodoEliminaciónGaussiana (matriz: float list list) (vectorResultados: float list) =
        // La matriz es cuadrada!
        assert (matriz.Length = matriz.[0].Length)
        // El vector tiene el largo de la matriz!
        assert (matriz.Length = vectorResultados.Length)
        
        Console.WriteLine("Matriz original ")
        Console.WriteLine(display2DListOfFloats matriz)
        
        let matrizConResultados = 
            matriz 
            |> List.mapi (fun índice fila ->
                fila @ [vectorResultados.[índice]]
            )
        Console.WriteLine("Matriz + resultados")
        Console.WriteLine(display2DListOfFloats matrizConResultados)

        let n = vectorResultados.Length
        
        let rec calcularMatrizDiagonal (matriz: float list list) (númeroDeFila: int) =
            Console.WriteLine($"Calculando la matriz diagonal, fila {númeroDeFila}. Matriz hasta ahora:")
            Console.WriteLine(display2DListOfFloats matriz)

            // Llegamos al final papá, no podemos hacer nada
            if númeroDeFila = n then
                matriz
            else
                // Elemento pivote de la fila actual
                let pivote = matriz.[númeroDeFila].[númeroDeFila]
                
                // Si el pivote es cero, no se avanza
                if pivote = 0.0 then
                    failwith "Elemento pivote es cero, se necesita implementar pivoteo"
                
                // Normalizar la fila actual (dividir toda la fila por el pivote)
                let filaActualNormalizada = 
                    matriz.[númeroDeFila] 
                    |> List.map (fun x -> x / pivote)
                
                // Actualizar la fila actual normalizada
                let matriz =
                    matriz
                    |> List.mapi (fun i fila -> 
                        if i = númeroDeFila then filaActualNormalizada else fila
                    )
                Console.WriteLine($"Calculando la matriz diagonal, fila {númeroDeFila}. Matriz con la fila actual normalizada:")
                Console.WriteLine(display2DListOfFloats matriz)
                
                // Eliminar el elemento en la columna actual de todas las demás filas
                let matriz =
                    matriz
                    |> List.mapi (fun i fila ->
                        if i <> númeroDeFila then
                            let factor = fila.[númeroDeFila]
                            fila 
                            |> List.mapi (fun j elemento ->
                                elemento - factor * filaActualNormalizada.[j]
                            )
                        else
                            fila
                    )
                
                // Continuar con la siguiente fila
                calcularMatrizDiagonal matriz (númeroDeFila + 1)
                
        // Comenzar la eliminación desde la primera fila (índice 0)
        let matrizDiagonal = calcularMatrizDiagonal matrizConResultados 0
        
        Console.WriteLine("Matriz diagonal!")
        Console.WriteLine(display2DListOfFloats matrizDiagonal)

        matrizDiagonal
    let calcularMatrizTriangular (matriz: float list list) (númeroDeFila: int) =
        let n = matriz.Length
        let rec calcularMatrizTriangularAux (matriz: float list list) (númeroDeFila: int) =
            Console.WriteLine $"Calculando la matriz triangular, fila {númeroDeFila}. Matriz hasta ahora:"
            Console.WriteLine (display2DListOfFloats matriz)

            // Llegamos al final papá, no hay nada más que hacer
            if númeroDeFila = n then
                matriz
            // Son todos ceros hasta la diagonal?
            else if matriz.[númeroDeFila].[0..númeroDeFila-1] |> List.forall (fun x -> x = 0.0) then
                // Seguimos, no hacemos nada
                calcularMatrizTriangularAux matriz (númeroDeFila + 1)
            else
                // Tomamos pivote, calculamos y coso
                let mutable matrizMutada = matriz
                for númeroDeColumna in 0 .. númeroDeFila-1 do
                
                    let pivote = matrizMutada.[númeroDeFila].[númeroDeColumna]
                    Console.WriteLine $"Pivote: {pivote}, Fila {númeroDeFila}, columna {númeroDeColumna}"
                    if pivote <> 0.0 then
                        let valorDeLaFilaDeArriba = matrizMutada.[númeroDeFila-1].[númeroDeColumna]
                        let valorPorElqueSeDebeDividirALaFilaDeArriba = valorDeLaFilaDeArriba / pivote
                        if valorPorElqueSeDebeDividirALaFilaDeArriba <> 0.0 then
                            let nuevaFilaDeArriba = matrizMutada.[númeroDeFila-1] |> List.map (fun x -> x / valorPorElqueSeDebeDividirALaFilaDeArriba)
                            Console.WriteLine $"Nueva fila de arriba:\n{display2DListOfFloats [matrizMutada.[númeroDeFila-1]]} / {valorPorElqueSeDebeDividirALaFilaDeArriba} = \n{display2DListOfFloats [nuevaFilaDeArriba]}"
                            matrizMutada <- 
                                matrizMutada
                                |> List.mapi (fun i fila -> 
                                    if i = númeroDeFila - 1 then
                                        nuevaFilaDeArriba
                                    else if i = númeroDeFila then
                                        fila |> List.mapi (fun j x -> x - nuevaFilaDeArriba.[j])
                                    else
                                        fila
                                )

                calcularMatrizTriangularAux matrizMutada (númeroDeFila + 1)

        calcularMatrizTriangularAux matriz 0

    let probarGauss() =
        let matriz = [
            [6.0; -2.0; 2.0; 4.0]
            [12.0; -8.0; 6.0; -1.0]
            [3.0; -13.0; 9.0; 3.0]
            [-6.0; 4.0; 1.0; -18.0]
        ]
        let vectorResultados = [12.0; 34.0; 27.0; -38.0]
        let matriz = [
            [6.0; -2.0; 2.0; 4.0]
            [12.0; -8.0; 6.0; 10.0]
            [3.0; -13.0; 9.0; 3.0]
            [-6.0; 4.0; 1.0; -18.0]
        ]
        let vectorResultados = [12.0; 34.0; 27.0; -38.0]
        
        //let resultado = métodoEliminaciónGaussiana matriz vectorResultados
        let resultado = calcularMatrizTriangular matriz 0
        Console.WriteLine("Matriz triangular!")
        Console.WriteLine(display2DListOfFloats resultado)
        ()

    