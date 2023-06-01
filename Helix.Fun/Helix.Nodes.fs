namespace Helix.Nodes

open System.Collections.Generic
open Blazor.Diagrams.Core
open Blazor.Diagrams.Core.Geometry
open Blazor.Diagrams.Core.Models

// Just what is this project doing in the C# one. Is it copied or linked indirectly?
// Linked indirectly.
// Let me commit here, and I'll erase the C# link.

type TextNode(p : Point) =
    inherit NodeModel(p)
    
    member val Text = "" with get, set

type CompilationNode(p : Point) =
    inherit NodeModel(p)
    
    member val Text = "" with get, set
    
type HelixPort(parent, alignment, isInput : bool) =
    inherit PortModel(parent, alignment, null, null)
    
    member val IsInput = isInput with get

    override this.CanAttachTo(port) =
        // This offset is because `this` already has a link by the time this function gets called.
        let is_input_port_empty (a : HelixPort) by = a.IsInput && a.Links.Count + by = 0 
        match port with
        | :? HelixPort as cp ->
            // Checks for same-node/port attachements.
            base.CanAttachTo(port)
            // Makes sure only input - output pairs are valid.
            && this.IsInput <> cp.IsInput
            // We also need to make sure that an input port only takes a single input.
            // && (is_input_port_empty this -1 || is_input_port_empty cp 0)
        | _ -> false

module Compilation =
    exception CycleException of NodeModel
    
    let inline memoize (dict : Dictionary<_, _>) f x =
        match dict.TryGetValue(x) with
        | true, v -> v
        | _ -> let y = f x in dict.Add(x,y); y
        
    let compile (start_node : CompilationNode) =
        let d = Dictionary()
        let ordered_nodes = ResizeArray()
        let visited_nodes = HashSet()
        let rec dfs node =
            memoize d (fun (node : NodeModel) ->
                if visited_nodes.Add node = false then raise (CycleException node)
                for port in node.Ports do
                    match port with
                    | :? HelixPort as port when port.IsInput ->
                        for link in port.Links do
                            dfs (if link.TargetNode <> node then link.TargetNode else link.SourceNode)
                    | _ -> ()
                    
                // Also adding the nodes really needs to be here.
                // It won't give the correct results if you put it in first place.
                ordered_nodes.Add node
                ) node
            
        dfs start_node
        ordered_nodes.ToArray()
