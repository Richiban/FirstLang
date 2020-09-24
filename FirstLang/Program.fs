module FirstLang.Program

open System
open FParsec

open FirstLang.Parsing

let input = """
where population > 1000000 & country == 'UK'
"""

match parse input with
| Result.Ok result -> printfn "%O" result
| Result.Error err -> printfn "%O" err
