namespace Practices.Math

module RaícesEcuacionesNoLineales =
    open Plotly.NET
    let e () = ()
    /// <summary>Teorema de Bolzano</summary>
    /// Sea ƒ : [a, b] → ℝ una función continua en [a, b] y supongamos que ƒ(a)·ƒ(b) < 0.
    /// Entonces ∃c ∈ [a, b] : ƒ(c) = 0.
    /// Estas son condiciones necesarias, pero no suficientes para garantizar la existencia de una raíz.

    /// <summary>Método de bisección</summary>
    /// Para una función ƒ : x → ℝ, para resolver ƒ(x) = 0.
    /// 1. Encontrar dos puntos a y b, tales que ƒ(a)·ƒ(b) < 0. Requiere estudio de la función.
    ///   1.1. Estudio de la función:
    ///         - Signo de la derivada: ƒ'(x) > 0 ⇒ ƒ(x) estrictamente creciente.
    ///         - Signo de la derivada: ƒ'(x) < 0 ⇒ ƒ(x) estrictamente decreciente.
    ///         - Si ƒ'(x) = 0 ⇒ ƒ(x) tiene un extremo relativo en x.
    /// 2. Generar puntos de la sucesión {x_n} de acuerdo al siguiente método:
    ///   2.1   x = (a + b) / 2.
    ///   2.2   Si ƒ(a)·ƒ(x) < 0 ⇒ x_2 = x, a = x.
    ///   2.3   Si ƒ(a)·ƒ(x) > 0 ⇒ x_2 = x, b = x.
    ///   2.4   Si ƒ(a)·ƒ(x) = 0 ⇒ x_2 = x.
    ///   2.5   Volver al paso 2.1.
    /// 3. Repetir el paso 2 hasta que |ƒ(x_n)| < ε.
    /// 4. La raíz es ≅ x_n.
    
    let rec métodoDeBisección (ƒ : float -> float) (a : float) (b : float) (epsilon : float)
        (xs : float list) =
        
        let x = a + (b - a) / 2.0

        let xOfƒ = ƒ x

        let xs = x :: xs

        if abs (xOfƒ) < epsilon then
            ([x], xs)
        else
            let aOfƒ = ƒ a
            if aOfƒ * xOfƒ < 0.0 then // es decir, la raíz está en [a, x]
                métodoDeBisección ƒ a x epsilon xs
            else // es decir, la raíz está en [x, b]
                métodoDeBisección ƒ x b epsilon xs
    
    /// Ejemplo:
    /// Para calcular el valor de √3, se puede usar el método de bisección.
    /// Se tiene que convertir el valor a una ecuación de la forma ƒ(x) = 0.
    /// Primero, x = √3.
    /// Luego, x² = 3.
    /// Por lo tanto, x² - 3 = 0.
    /// ƒ(x) = x² - 3. (Es decir, tenemos certeza de que √3 es una raíz de esta función)
    let calcularRaizCuadradaDe3 () =
     
        let ƒ x = x * x - 3.0

        /// Salteamos el estudio de la función, es trivial.
        let a = 1.0
        let b = 2.0
        let epsilon = 0.001

        let (raices, xs) = métodoDeBisección ƒ a b epsilon []
        /// Gráfico de la función y los sucesivos x_n:
        let original = Chart.Line(xy = [ for x in [a .. 0.01 .. b] -> (x, ƒ x) ])
        let plot = Chart.Scatter(xy = [ for x in xs -> (x, ƒ x) ], mode = StyleParam.Mode.Markers)
                    |> Chart.withLineStyle(Color = Color.fromHex "#FF0000")
                    |> Chart.withXAxisStyle(Title = Title.init("x"), Color = Color.fromHex "#000000")
        let raíz = Chart.Scatter(xy = [ for x in raices -> (x, 0.0) ], mode = StyleParam.Mode.Markers)
                    |> Chart.withLineStyle(Color = Color.fromHex "#00FF00")
        let plot = [original; plot; raíz] |> Chart.combine
        plot |> Chart.show

        ()
    
    /// <summary>Método de punto fijo</summary>        
    /// Sea g : D_g -> ℝ con D_g ⊆ ℝ, se dice que q ∈ D_g es un punto fijo de g si g(q) = q.
    /// Es decir, una funciòn g con un dominio D_g, tiene un punto fijo q, si al aplicarle la función g, da como resultado q.
    ///
    /// Método:
    /// 1. Partir de la ecuación ƒ(x) = 0.
    /// 2. Despejar x en función de x: x = g(x).
    /// 3. Iterar: x_{n+1} = g(x_n) para n ≥ 0.
    /// 4. Repetir hasta que |x_{n+1} - x_n| < ε.
    /// 5. La raíz es ≅ x_n.
    /// 
    /// Este método solo es aplicable si g converge.
    /// Condición de existencia y unicidad de la raíz:
    /// Si x = g(x) continua en [a, b] tal que g(x) [a,b] ∀x ∈ [a,b],
    /// y |g'(x)| ≤ K < 1 ∀x ∈ [a,b], entonces g tiene un único punto fijo en [a,b].
    /// Condición suficiente de convergencia:
    /// Si x = g(x) continua en [a, b] tal que g(x) [a,b] ∀x ∈ [a,b],
    /// y |g'(x)| ≤ K < 1 ∀x ∈ [a,b]. Si x₀ ∈ [a,b], entonces la sucesión
    /// {x_n} definida por x_{n+1} = g(x_n) converge al único punto fijo ɑ de g en [a,b].
    /// 
    let rec métodoDePuntoFijo (g : float -> float) (x_0 : float) (epsilon : float)
        (xs : float list) =
        
        let x = g x_0

        let xs = x :: xs

        if abs (x - x_0) < epsilon then
            ([x], xs)
        else
            métodoDePuntoFijo g x epsilon xs

    /// Ejemplo:
    /// ƒ(x) = x³ - 7x + 2
    /// Se debe usar un intervalo [a,b] que contenga una raíz de la función.
    /// Usamos [0,1]
    /// 
    /// ƒ(a) = ƒ(0) = 2
    /// ƒ(b) = ƒ(1) = -4
    /// ƒ(a)·ƒ(b) = ƒ(0)·ƒ(1) < 0, por lo tanto, existe una raíz en el intervalo [0,1]. (Teorema de Bolzano)
    /// 
    /// Obtenemos g(x):
    /// x = g(x)
    /// x³ - 7x + 2 = x
    /// x³ - 8x + 2 = 0
    /// g(x) = (x³ + 2) / 8
    /// 
    /// Obtenemos g'(x):
    /// g(x) = (x³ + 2) / 8
    /// g(x) = x³ /8 + 2 /8
    /// g'(x) = (x³ /8)' + (2 /8)'
    /// g'(x) = 3x² /8 + 0
    /// 
    /// g(a) = g(0) = 2 / 8
    /// g(b) = g(1) = 3 / 8
    /// 
    /// |g'(x)| ≤ 3/8 < 1 ∀x ∈ [0,1]
    /// 
    /// 
    /// 
    let calcularRaizDeƒConPuntoFijo () =

        let ƒ x = x * x * x - 7.0 * x + 2.0

        let g x = (x * x * x + 2.0) / 7.0

        let g' x = (3.0 * x * x) / 7.0

        let a = 0.0
        let b = 1.0
        let epsilon = 0.000001

        let (raíces, xs) = métodoDePuntoFijo g a epsilon []

        let original = Chart.Line(xy = [ for x in [a .. 0.01 .. b] -> (x, ƒ x) ])
        let plot = Chart.Scatter(xy = [ for x in xs -> (x, ƒ x) ], mode = StyleParam.Mode.Markers)
                    |> Chart.withLineStyle(Color = Color.fromHex "#FF0000")
        let raíz = Chart.Scatter(xy = [ for x in raíces -> (x, ƒ x) ], mode = StyleParam.Mode.Markers)
                    |> Chart.withLineStyle(Color = Color.fromHex "#00FF00")
        let plot = [original; plot; raíz] |> Chart.combine
        plot |> Chart.show

    /// <summary>Método de Newton-Raphson</summary>
    /// Se busca hallar una secusión {x_n} que converja a una raíz ɑ : ƒ(ɑ) = 0
    /// en las intersecciones con el eje x de las sucesivas retas tangentes.
    /// 
    /// Método:
    /// 1. Tomar ecuacion de la recta tangente a ƒ en un punto ɑ:
    ///    y - ƒ(ɑ) = ƒ'(ɑ)(x - ɑ)
    /// 2. Encontrar la intersección de la recta tangente con el eje x:
    ///    y = 0
    ///    y - ƒ(ɑ) = ƒ'(ɑ)(x - ɑ)
    ///    0 - ƒ(ɑ) = ƒ'(ɑ)(x - ɑ)
    ///    x = ɑ - ƒ(ɑ) / ƒ'(ɑ)
    /// 3. Iterar: x_{n+1} = x_n - ƒ(x_n) / ƒ'(x_n) para n ≥ 0.
    /// 
    /// Con las siguientes condiciones:
    /// 1. ƒ(ɑ)·ƒ(b) ≥ 0
    /// 2. ƒ es continua en [ɑ,b]
    /// 3. ƒ' es continua en (ɑ,b)
    /// 
    /// 
    /// 
    /// 
    /// 


