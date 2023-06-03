namespace Helix.Nodes

open System
open System.Collections.Generic
open Blazor.Diagrams.Core
open Blazor.Diagrams.Core.Geometry
open Blazor.Diagrams.Core.Models
open Blazor.Diagrams.Core.Models.Base
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components.Web

type HelixPort(parent : NodeModel, alignment, isInput : bool) =
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

type TextNode(p : Point) as node =
    inherit NodeModel(p)
    
    do node.AddPort(HelixPort(node, PortAlignment.Left, true)) |> ignore
    do node.AddPort(HelixPort(node, PortAlignment.Right, false)) |> ignore
    
    member val Text = "" with get, set
    
type ImageNode(p : Point) as node =
    inherit NodeModel(p)
    
    do node.AddPort(HelixPort(node, PortAlignment.Left, true)) |> ignore
    do node.AddPort(HelixPort(node, PortAlignment.Right, false)) |> ignore
    
    member val Src = "images/sun-big.png" with get, set
    
type DatabaseTestNode(p : Point) =
    inherit NodeModel(p)
    
type CompilationNode(p : Point) as node =
    inherit NodeModel(p)
    
    do node.AddPort(HelixPort(node, PortAlignment.Left, true)) |> ignore // It doesn't have an output port.
    
    member val ChildComponent : RenderFragment IEnumerable = [||] with get, set
    

module Utils =
    let add_node (d : Diagram, node_constructor : Func<Point, NodeModel>) (ev : MouseEventArgs) =
        let to_canvas_point (x,y) = d.GetRelativeMousePoint(x,y)
        d.Nodes.Add(node_constructor.Invoke(to_canvas_point(ev.ClientX, ev.ClientY)))
        
module StoreLoad =
    open System.Text.Json
    type StoredNodes =
        | S_TextNode of string
        | S_ImageNode of string
        | S_CompilationNode
        | S_DatabaseTestNode
        
    type NodeT = StoredNodes * (double * double)
    type LinkT = int * int
    
    let private diagram_ids (nodes : NodeModel seq) =
        let d_ports_from = Dictionary(HashIdentity.Reference)
        let d_ports_to = Dictionary()
        
        nodes |> Seq.iter (fun x ->
            x.Ports |> Seq.iter (fun x ->
                let i = d_ports_from.Count
                d_ports_from.Add(x,i) // Maps the ports to unique ids.
                d_ports_to.Add(i,x) // Maps the unique ids to ports
                )
            )
        d_ports_from, d_ports_to
    
    let store (diagram : Diagram) =
        let d_ports_from, d_ports_to = diagram_ids diagram.Nodes
        
        let to_tuple (p : Point) = p.X, p.Y
        let nodes =
            diagram.Nodes
            |> Seq.toArray
            |> Array.map (function
                | :? TextNode as n -> S_TextNode n.Text, to_tuple n.Position
                | :? ImageNode as n -> S_ImageNode n.Src, to_tuple n.Position
                | :? CompilationNode as n -> S_CompilationNode, to_tuple n.Position
                | :? DatabaseTestNode as n -> S_DatabaseTestNode, to_tuple n.Position
                | n -> failwithf $"Type not supported: {n.GetType()}"  
                )
            |> fun (x : NodeT []) -> Thoth.Json.Net.Encode.Auto.toString x
        
        let links =
            diagram.Links
            |> Seq.toArray
            |> Array.map (fun x ->
                d_ports_from[x.SourcePort], d_ports_from[x.TargetPort]
                )
            |> fun (x : LinkT []) -> Thoth.Json.Net.Encode.Auto.toString x
            
        nodes, links
        
    let load (d : Diagram) (nodes : string) (links : string) =
        d.Nodes.Clear()
        d.Links.Clear()
        
        let nodes =
            (Thoth.Json.Net.Decode.Auto.unsafeFromString nodes : NodeT [])
            |> Array.map (function
                | S_TextNode s, p -> TextNode(Point p, Text=s) : NodeModel
                | S_ImageNode s, p -> ImageNode(Point p, Src=s)
                | S_CompilationNode, p -> CompilationNode(Point p)
                | S_DatabaseTestNode, p -> DatabaseTestNode(Point p) 
                )
            
        let d_ports_from, d_ports_to = diagram_ids nodes
        
        let links =
            (Thoth.Json.Net.Decode.Auto.unsafeFromString links : LinkT [])
            |> Array.map (fun (source, target) ->
                LinkModel(d_ports_to[source],d_ports_to[target]) :> BaseLinkModel
                )
        d.Nodes.Add nodes; d.Links.Add links
            
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
