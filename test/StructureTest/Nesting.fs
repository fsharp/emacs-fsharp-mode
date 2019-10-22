// This file contains hand-crafted structures for use by `fsharp-mode-structure-tests.el`.
// In particular, many/most of those tests need to work by:
//
//   1. Inserting text in a temp buffer
//   2. Moving point to a known position
//   3. Comparing computed values against expected answers
//
// Frequently, we're comparing things like, "what is the exact (point) position
// of a given square brace." This means that formatting changes to this buffer
// -- indeed, edits _of any kind_ -- will almost certainly break the tests! Edit
// thoughtfully and intentionally! Update things as needed!

// (point) of opening [: 640
let aList = [ 1; 2; 3]

// (point) of inner opening [: 706
let nestedList = [ [ "this"; "that" ] ]

// (point) of opening [: 777
let multiLineList = [
    "this"
    "that"
]

// (point) of outermost opening [: 947
// (point) of middle opening [:    953
// (point) of innermost opening [: 955
let multiLineNestedList = [
    [ [ "how"; "now"]
    ]
]

// (point) of opening {: 1060
// (point) of inner {:   1121
let anAsync = async {
    let value = funCall()

    let! differentValue = async { return! 5 }
}

// (point) of opening (: 1208
let thing =
    [ 1; 2]
    |> List.map (fun i ->
                 i ** i )
