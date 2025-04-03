open System



let calculate (op: string) (x: float) (y: float) =
    match op with
    | "+" -> x + y
    | "-" -> x - y
    | "*" -> x * y
    | "/" -> 
        if y <> 0.0 then x / y
        else 
            Console.ForegroundColor <- ConsoleColor.Red
            printfn "Cannot divide by zero."
            Console.ResetColor()
            Double.NaN
    | "pow" -> Math.Pow(x, y)
    | "sqrt" -> 
        if x >= 0.0 then Math.Sqrt(x)
        else 
            Console.ForegroundColor <- ConsoleColor.Red
            printfn "Cannot take the square root of a negative number."
            Console.ResetColor()
            Double.NaN
    | "sin" -> Math.Sin(x * Math.PI / 180.0)
    | "cos" -> Math.Cos(x * Math.PI / 180.0)
    | "tan" -> 
        let radians = x * Math.PI / 180.0
        if (x % 180.0 = 90.0) then
            Console.ForegroundColor <- ConsoleColor.Red
            printfn "Tangent is undefined for odd multiples of 90 degrees."
            Console.ResetColor()
            Double.NaN
        else Math.Tan(radians)
    | _ -> 
        Console.ForegroundColor <- ConsoleColor.Red
        printfn "Invalid operation."
        Console.ResetColor()
        Double.NaN



let rec getValidFloat prompt =
    printf "%s" prompt
    let input = Console.ReadLine()
    match Double.TryParse(input) with
    | (true, value) -> value
    | (false, _) ->
        Console.ForegroundColor <- ConsoleColor.Yellow
        printfn "Invalid number format. Please try again."
        Console.ResetColor()
        getValidFloat prompt



let rec getValidOperator prompt =
    printf "%s" prompt
    let input = Console.ReadLine()
    match input with
    | "+" | "-" | "*" | "/" | "pow" | "sqrt" | "sin" | "cos" | "tan" -> input
    | _ ->
        Console.ForegroundColor <- ConsoleColor.Yellow
        printfn "Invalid operator. Please enter one of +, -, *, /, pow, sqrt, sin, cos, tan."
        Console.ResetColor()
        getValidOperator prompt



[<EntryPoint>]
let main argv =
    let mutable history = []
    let mutable running = true

    while running do
        printfn "What would you like to do? (Enter a corresponding number to perform action)"
        printfn "1. Write Math equation."
        printfn "2. View History."
        printfn "0. Exit program."
        
        printf "Enter a number: "
        let choice = Console.ReadLine()
        
        match choice with
        | "1" ->
            let x = getValidFloat "Enter first number: "
            let op = getValidOperator "Enter an operator (+, -, *, /, pow, sqrt, sin, cos, tan): "
            let y = if op <> "sqrt" && op <> "sin" && op <> "cos" && op <> "tan" then getValidFloat "Enter second number: " else 0.0
            
            let result = calculate op x y
            Console.ForegroundColor <- ConsoleColor.Green
            printf "Result: "
            Console.ForegroundColor <- ConsoleColor.Blue
            printfn "%.3f" result
            Console.ResetColor()

            history <- (x, op, y, result) :: history
        | "2" ->
            printfn "Calculation History:"
            history |> List.iter (fun (x, op, y, result) -> printfn "%.3f %s %.3f = %.3f" x op y result)
        | "0" ->
            running <- false
        | _ ->
            Console.ForegroundColor <- ConsoleColor.Yellow
            printfn "Invalid choice. Please try again."
            Console.ResetColor()

    0