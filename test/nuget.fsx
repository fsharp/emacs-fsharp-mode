#r "nuget: Newtonsoft.Json";;
open Newtonsoft.Json;;

let o = {| X = 2; Y = "Hello" |};;

printfn "xxx:%s:xxx" (JsonConvert.SerializeObject o);;
