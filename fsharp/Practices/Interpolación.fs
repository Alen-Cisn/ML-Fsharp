namespace Practices

module Interpolación =
//    open Plotly.NET
    let pi = 3.14159

    let rec fact n = 
        if n = 0. then 1.
        else n * fact (n - 1.)

    let ℓ xs j (x: float) = 
        let up = 
            xs
            |> Array.mapi (fun i x -> i,x )
            |> Array.filter (fun pair -> fst pair <> j)
            |> Array.fold (fun producto pair -> (x - snd pair) * producto) 1.

        let down =
            xs
            |> Array.mapi (fun i x -> i,x )
            |> Array.filter (fun pair -> fst pair <> j)
            |> Array.fold (fun producto pair -> (xs.[j] - snd pair) * producto) 1.

        up / down
        
    let L nodos ys x =
            ys
            |> Array.mapi (fun i yi -> yi * (ℓ nodos i x)) 
            |> Array.sum

    let punto2() =
        let nodos = [|2.0; 3.0; 4.0|] 
        let ℓ = ℓ nodos

        let f (x: float) = 1.0 - sqrt(x) / (x * x) 
        let ys = nodos |>    Array.map f 
        
        let Psub2 (x: float) = L nodos ys x
        
        printfn "Nodos: %A" nodos
        printfn "Ys: %A" ys
        let x_eval = 2.5
        printfn "P₂(%f) = %f" x_eval (Psub2 x_eval)
        printfn "f(%f) = %f" x_eval (f x_eval)

        let f''' x = 3./8. * (35. * sqrt x - 64.) / pown x 5

        let E x = abs((f x) - (Psub2 x))

        // Create a graph comparing the original function and the interpolation polynomial
        let xRange = [| 1.8 .. 0.01 .. 4.2 |] // Range slightly wider than our nodes
        
        // Create data points for both functions
        let originalFunctionPoints = xRange |> Array.map (fun x -> (x, f x))
        let interpolationPoints = xRange |> Array.map (fun x -> (x, Psub2 x))
        let nodesPoints = nodos |> Array.map (fun x -> (x, f x))
        ()
        
    let punto3() =
        let xInterpol = 0.6
        let nodos = [| 0.4; 0.5; 0.7; 0.8|] 
        let ℓ = ℓ nodos

        let f (x: float) = log x
        let ys = nodos |> Array.map f 
        
        let Psub3 (x: float) = 
            Array.mapi (fun i yi -> yi * (ℓ i x)) ys
            |> Array.sum
        
        printfn "Nodes: %A" nodos
        printfn "Y values: %A" ys

        printfn "P₃(%f) = %f" xInterpol (Psub3 xInterpol)
        printfn "f(%f) = %f" xInterpol (f xInterpol)


        let E x = abs((f x) - (Psub3 x))

        let f'''' x = -6. / pown x 4

        let interm x = 
            nodos
            |> Array.map (fun xi -> x - xi )
            |> Array.fold (fun producto xi -> xi * producto) 1.

        let Elimit x e =  (abs(f'''' e) / (fact (float nodos.Length))) * interm x

        ()

    let punto4() = 
        let f x = x + 2./x
        let xAAproximar = [|1.5; 1.2|]
        let nodos = [|1.; 2.; 2.5|] 
        
        let ys = nodos |> Array.map f 
        
        let Psub4 (x: float) = L nodos ys x

        
        let E x = abs((f x) - (Psub4 x))

        let f''' x = -12. / pown x 4

        let interm x = 
            nodos
            |> Array.map (fun xi -> x - xi )
            |> Array.fold (fun producto xi -> xi * producto) 1.

        let Elimit x e =  (abs(f''' e) / (fact (float nodos.Length))) * interm x


        ()
        
    let w b a x = (b + a) / 2 + (b - a) / 2 * x
    let x b a x = (b + a) / 2 + (b - a) / 2 * x

    let punto4b() = 
        let nodos = [|0.5; 1; 2|]
        let n = float nodos.Length
        let xs = 
            nodos
            |> Array.mapi (fun k xk -> cos((n * (float k) + 1.)/(n * (n + 1.))* pi)  )

        ()

    let punto6() =
        let nodos = [||]
        ()

    let punto8() =
        let nodos = [||]
        ()

    //Int. de Newton 
    let punto8b() =
        let límitesIntervalo = (0., 3.)
        let cantidadNodos = 3
        //let nodos = [|2.799; 1.5;0.2009|]
        let f x = 1. + (1./6.) * x * (6. + 3. * x + 5. * pown x 2) + sinh ( 1. * x )

        // Calcular los nodos de Chebyshev para el intervalo [a,b]
        let a, b = límitesIntervalo
        let nodosCheby = 
            [|0 .. cantidadNodos-1|]
            |> Array.map (fun i -> 
                // Fórmula de nodos de Chebyshev: x_i = (a+b)/2 + (b-a)/2 * cos(π*(2i+1)/(2n))
                let i = float i
                let n = float cantidadNodos
                (a + b) / 2. + (b - a) / 2. * cos(pi * (2. * i + 1.) / (2. * n))
            )
        
        printfn "Nodos de Chebyshev para el intervalo [%f, %f]:" a b
        nodosCheby |> Array.iteri (fun i x -> printfn "x_%d = %f" i x)
        (*
        // Calcular los valores de la función en los nodos
        let ys = nodosCheby |> Array.map f
        printfn "Valores de la función en los nodos:"
        Array.zip nodosCheby ys |> Array.iteri (fun i (x, y) -> printfn "f(%f) = %f" x y)
        
        // Definir el polinomio de interpolación de Lagrange usando los nodos de Chebyshev
        let Psub8 (x: float) = L nodosCheby ys x
        
        // Evaluar el polinomio en algunos puntos para comprobar
        let puntosTest = [|0.5; 1.5; 2.5|]
        printfn "\nEvaluación del polinomio interpolador:"
        puntosTest |> Array.iter (fun x -> 
            printfn "P(%f) = %f, f(%f) = %f, Error = %f" 
                x (Psub8 x) x (f x) (abs((f x) - (Psub8 x)))
        )

        // Cálculo de los coeficientes del polinomio usando diferencias divididas de Newton
        let calcularDiferenciasDivididas (xs: float[]) (ys: float[]) =
            let n = xs.Length
            let dd = Array2D.create n n 0.0
            
            // Inicializar la primera columna con los valores de y
            for i = 0 to n-1 do
                dd.[i, 0] <- ys.[i]
            
            // Calcular las diferencias divididas
            for j = 1 to n-1 do
                for i = 0 to n-j-1 do
                    dd.[i, j] <- (dd.[i+1, j-1] - dd.[i, j-1]) / (xs.[i+j] - xs.[i])
            
            // Extraer los coeficientes (primera fila de la matriz)
            [| for j = 0 to n-1 -> dd.[0, j] |]
        
        let coeficientes = calcularDiferenciasDivididas nodosCheby ys
        
        // Mostrar los coeficientes
        printfn "\nCoeficientes del polinomio interpolador (forma de Newton):"
        coeficientes |> Array.iteri (fun i a -> printfn "a_%d = %f" i a)
        
        // Función para evaluar el polinomio en forma de Newton
        let evaluarNewton x =
            let mutable resultado = coeficientes.[0]
            let mutable producto = 1.0
            
            for i = 1 to coeficientes.Length - 1 do
                producto <- producto * (x - nodosCheby.[i-1])
                resultado <- resultado + coeficientes.[i] * producto
            
            resultado
        
        // Convertir a forma estándar (a_0 + a_1*x + a_2*x^2 + ...)
        // Esto es más complejo y requeriría multiplicar los términos (x-x_0)(x-x_1)... y agrupar
        // Para simplificar, podemos usar un método numérico para aproximar los coeficientes
        
        let aproximarCoeficientesEstandar() =
            // Creamos un sistema de ecuaciones lineales
            // Evaluamos el polinomio en n puntos y resolvemos para los coeficientes
            let n = nodosCheby.Length
            let matrizVandermonde = Array2D.create n n 0.0
            
            for i = 0 to n-1 do
                let x = nodosCheby.[i]
                for j = 0 to n-1 do
                    matrizVandermonde.[i, j] <- pown x j
            
            // Resolver sistema Ax = b (usando eliminación Gaussiana simplificada)
            // Nota: esto es una simplificación, en la práctica usaríamos una biblioteca más robusta
            let resolverSistema (A: float[,]) (b: float[]) =
                let n = b.Length
                let x = Array.copy b
                let A = Array2D.copy A  // Copiamos para no modificar el original
                
                // Eliminación hacia adelante
                for k = 0 to n-2 do
                    for i = k+1 to n-1 do
                        let factor = A.[i, k] / A.[k, k]
                        for j = k+1 to n-1 do
                            A.[i, j] <- A.[i, j] - factor * A.[k, j]
                        x.[i] <- x.[i] - factor * x.[k]
                
                // Sustitución hacia atrás
                for i = n-1 downto 0 do
                    let mutable suma = 0.0
                    for j = i+1 to n-1 do
                        suma <- suma + A.[i, j] * x.[j]
                    x.[i] <- (x.[i] - suma) / A.[i, i]
                
                x
            
            let coeficientesEstandar = resolverSistema matrizVandermonde ys
            coeficientesEstandar
        
        let coefEstándar = aproximarCoeficientesEstandar()
        
        printfn "\nCoeficientes del polinomio en forma estándar (a_0 + a_1*x + a_2*x^2 + ...):"
        coefEstándar |> Array.iteri (fun i a -> printfn "a_%d = %f" i a)
        
        // Función para evaluar el polinomio en forma estándar
        let evaluarEstandar x =
            coefEstándar |> Array.mapi (fun i a -> a * pown x i) |> Array.sum
        
        // Comparar las diferentes formas de evaluación
        printfn "\nComparación de las diferentes formas del polinomio:"
        puntosTest |> Array.iter (fun x -> 
            printfn "En x=%f: Lagrange=%f, Newton=%f, Estándar=%f, f(x)=%f" 
                x (Psub8 x) (evaluarNewton x) (evaluarEstandar x) (f x)
        )*)

        //let rec F (xs: float[]) n =  
        //    if xs.Length = 1 then f xs.[0]
        //    else 
        //        F (xs |> Array.mapi (fun i x -> i, x ) |> Array.filter (fun pair -> 0 <> fst pair) |> Array.map (fun pair -> snd pair))
        //        - F (xs |> Array.mapi (fun i x -> i, x ) |> Array.filter (fun pair -> 0 <> fst pair) |> Array.map (fun pair -> snd pair))

        //let ys = nodos |> Array.map f
        //let P As Xs x =
        //    As

        ()