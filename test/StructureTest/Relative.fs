type Test  =
    | Unit
    | Integration of string
    | EndToEnd


if thing <> true then
    printfn "thing is not true"
else if thing = true
then
    printfn "maybe?"
else
    printfn "it is so"


let aThing (test : Test) = function
    | Unit -> ()
    | Integration -> ()
    | EndToEnd -> ()
