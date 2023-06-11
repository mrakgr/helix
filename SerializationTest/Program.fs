open System
open System.Buffers
open MessagePack
open MessagePack.Resolvers
open MessagePack.FSharp

[<MessagePackObject true>]
type R = {placeholder : bool}

[<MessagePackObject>]
type Qwe =
    | Asd of R
    | Zxc of {|a : string; b : int|}
    
// Thoth.Json.Net.Encode.Auto.toString (Asd {|placeholder=true; qwe=Some 123; asd=Some 123|} )
// |> printfn "%s"
//

Thoth.Json.Net.Decode.Auto.fromString<Qwe> """ ["Asd",{"placeholder":true,"asd":123}] """
|> printfn "%A"

// let convertAsMemory<'T> options (value: 'T) =
//   let memory = (MessagePackSerializer.Serialize(value, options))
//   let memory = [|146uy; 0uy; 145uy; 130uy; 171uy; 112uy; 108uy; 97uy; 99uy; 101uy; 104uy; 111uy;
//       108uy; 100uy; 101uy; 114uy; 194uy; 163uy; 113uy; 119uy; 101uy; 165uy; 72uy;
//       101uy; 108uy; 108uy; 111uy|]
//
//   // printfn "%A" memory
//   printfn "%A" (System.Text.Encoding.Default.GetString memory)
//   MessagePackSerializer.Deserialize<'T>(memory, options)
//
// let resolver =
//   CompositeResolver.Create(
//     StandardResolver.Instance,
//     FSharpResolver.Instance
//     )
//   
//
// let options = MessagePackSerializerOptions.Standard.WithResolver(resolver)
//
// Asd {placeholder=false; qwerty=123}
// // |> convertAsSequence options
// |> convertAsMemory options
// |> printfn "%A"


