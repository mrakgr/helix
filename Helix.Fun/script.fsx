match box (null : string) with
| :? string -> printfn "string"
| _ -> printfn "else"