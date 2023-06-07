open System.Collections.Generic

let a = ['a'; 'b'; 'c']
let b = ['a'; 'b']
let ha = HashSet(a)
let hb = HashSet(b)
ha.ExceptWith hb
printfn "%A" ha