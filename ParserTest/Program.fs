open FParsec
open System

type A = A of css: string * text: B
and B = B of (string * A option) list

let css_class str =
    let f1 x = Char.IsLetter x || x = '_'
    let f x = f1 x || x = '-' || Char.IsDigit x
    many1Satisfy2L f1 f "A string made of letters, '_', '-' and digits, that starts with a letter or a '_'." str
let rec p_a str : Reply<A> = (pchar '%' >>. (css_class .>>. between (pchar '{') (pchar '}') (p_b (Set.singleton '}'))) |>> A) str
and p_b (ex : _ Set) str : Reply<B> =
    (manyCharsTillApply anyChar ((p_a |>> Some) <|> ((eof <|> followedBy (choice (Seq.map skipChar ex))) >>% None)) (fun a b -> a,b) >>= function
        | (a,Some _ & b) -> p_b ex |>> fun (B r) -> B((a,b) :: r)
        | x -> preturn (B [x])
        ) str
    
let gen speech thought x =
    let rec gen_b (B l) =
        l |> List.map (fun (text,a) ->
            match a with
            | Some a -> $"%s{text}%s{gen_a a}"
            | None -> text
            )
        |> String.concat ""
    and gen_a (A(css,b)) =
        let span = "span"
        let b =
            let b = gen_b b
            if Set.contains css speech then $"\"{b}\""
            elif Set.contains css thought then $"'{b}'"
            else b
        $"<{span} class=\"helix-text %s{css}\">%s{b}</{span}>"
    gen_b x
      
// let example = "ab%qwe{zxc%rrr{ttt}99%_987{9}}123%zzz{yyy}"
let example =
    """
%b{Hello.} Darwin said.

%bi{I must go forward.} The thought sprang from his heart....
"""
runParserOnString (p_b Set.empty |>> gen (Set.singleton "b") (Set.singleton "bi")) () "ghostlike" example 
|> printfn "%A"