let notABlock = 5

let basicBlock =
    [ 1; 2; 3 ]
    |> List.fold (fun x y -> x + y)

type Shape =
    | Square
    | Rectangle
    | Triangle

let aFunction x y =
    if x < y
    then
        x
    else
        y

