namespace Helix.Nodes

open System
open System.Collections.Generic
open Blazor.Diagrams.Core
open Blazor.Diagrams.Core.Geometry
open Blazor.Diagrams.Core.Models
open Blazor.Diagrams.Core.Models.Base
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components.Forms
open Microsoft.AspNetCore.Components.Web
open Microsoft.JSInterop

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
        
type UndoBufferActions =
    | U_NodeAdded of NodeModel
    | U_NodeRemoved of NodeModel
    | U_LinkAdded of BaseLinkModel
    | U_LinkRemoved of BaseLinkModel
    | U_NodesMoved of start: Dictionary<NodeModel,Point> * ``end``: Dictionary<NodeModel,Point>
        
open FSharp.Control.Reactive
type HelixDiagramBase(Js : IJSRuntime, opts) as this =
    inherit Diagram(opts)
    
    let redo = Stack()
    let undo = Stack()
    
    let mutable is_loaded = false
    let mutable is_handler_active = true
    
    let handler_template f x =
        if is_handler_active then
            undo.Push(f x)
            redo.Clear()
    do 
        this.Nodes.add_Added (handler_template U_NodeAdded); this.Nodes.add_Removed (handler_template U_NodeRemoved)
        this.Links.add_Added (handler_template U_LinkAdded); this.Links.add_Removed (handler_template U_LinkAdded)
        
    do this.add_KeyDown (fun ev -> 
        match ev.Key.ToLower() with
        | "z" when ev.CtrlKey && ev.ShiftKey -> this.Redo()
        | "z" when ev.CtrlKey -> this.Undo()
        | _ -> ()
        )
    
    do
        // let inline g add remove = Observable.fromEventGeneric (fun f -> add (fun a b -> f (a,b))) (fun f -> remove (fun a b -> f (a,b)))
        let inline fromEvent add remove =
            let inline g (h : Action<_,_> -> unit) (f : Action<_>) = h (Action<_,_>(fun a b -> f.Invoke (a,b)))
            System.Reactive.Linq.Observable.FromEvent<_>(g add, g remove)
        let mouse_down = fromEvent this.add_MouseDown this.remove_MouseDown
        let mouse_up = fromEvent this.add_MouseUp this.remove_MouseUp
        let nodes_moved : UndoBufferActions IObservable =
            mouse_down |> Observable.switchMap (fun _ ->
                let nodes_movement_start = this.Nodes |> Seq.choose (fun x -> if x.Selected then Some (KeyValuePair(x, x.Position)) else None) |> Dictionary
                mouse_up |> Observable.first |> Observable.choose (fun _ ->
                    let nodes_movement_end = nodes_movement_start |> Seq.map (fun (KeyValue(x,_)) -> KeyValuePair(x, x.Position)) |> Dictionary
                    let has_changed = Seq.exists2 (fun (KeyValue(_,p1)) (KeyValue(_,p2)) -> p1 <> p2) nodes_movement_start nodes_movement_end
                    if has_changed then Some (U_NodesMoved(nodes_movement_start,nodes_movement_end)) else None
                    )
                )
        Observable.subscribe undo.Push nodes_moved |> ignore
        
    
    let nodes_moved d = for KeyValue(k : NodeModel,v) in d do k.Position <- v; k.RefreshAll(); k.ReinitializePorts()
    
    member this.Undo() =
        if undo.Count > 0 then
            is_handler_active <- false
            let act = undo.Pop()
            redo.Push(act)
            match act with
            | U_NodeAdded x -> this.Nodes.Remove x
            | U_NodeRemoved x -> this.Nodes.Add x
            | U_LinkAdded x -> this.Links.Remove x
            | U_LinkRemoved x -> this.Links.Add x
            | U_NodesMoved(start, ``end``) -> nodes_moved start
            is_handler_active <- true
            
    member this.Redo() =
        if redo.Count > 0 then
            is_handler_active <- false
            let act = redo.Pop()
            undo.Push(act)
            match act with
            | U_NodeAdded x -> this.Nodes.Add x
            | U_NodeRemoved x -> this.Nodes.Remove x
            | U_LinkAdded x -> this.Links.Add x
            | U_LinkRemoved x -> this.Links.Remove x
            | U_NodesMoved(start, ``end``) -> nodes_moved ``end``
            is_handler_active <- true
            
    member this.OnLoad() = task {
        if not is_loaded then
            do! Js.InvokeVoidAsync("registerUnloadEvent", DotNetObjectReference.Create(this));            
            is_loaded <- true
        
        let! nodes = Js.InvokeAsync<string>("localforage.getItem", "diagram_nodes")
        let! links = Js.InvokeAsync<string>("localforage.getItem", "diagram_links") 
        StoreLoad.load this nodes links
    }
    
    member this.OnStore() = task {
        if is_loaded then
            let (nodes, links) = StoreLoad.store this
            do! Js.InvokeVoidAsync("localforage.setItem", "diagram_nodes", nodes);
            do! Js.InvokeVoidAsync("localforage.setItem", "diagram_links", links);
    }
    
    [<JSInvokable>]
    member this.OnBeforeUnload() = this.OnStore()
    
open FSharp.Control
type ReactiveURLs(Js : IJSRuntime) =
    let files = AsyncRx.subject()
    let urls = AsyncRx.subject()
    
    static let allowed_schemas = [|Uri.UriSchemeHttp; Uri.UriSchemeHttps; Uri.UriSchemeFile; Uri.UriSchemeFtp; Uri.UriSchemeFtps|]
    
    let uplaod (file : IBrowserFile) = (fst files).OnNextAsync file
    
    let files_obs =
        (snd files)
        |> AsyncRx.map (fun file ->
            AsyncRx.create (fun obs -> async {
                use dotnetImageStream = new DotNetStreamReference(file.OpenReadStream(file.Size))
                let! url = Js.InvokeAsync<string>("createObjectURL_FromStream", dotnetImageStream, file.ContentType).AsTask() |> Async.AwaitTask
                do! obs.OnNextAsync url
                return AsyncDisposable.Create (fun () -> Js.InvokeVoidAsync("URL.revokeObjectURL", url).AsTask() |> Async.AwaitTask)
            })
        ) |> AsyncRx.switchLatest
        
    let is_url url =
        let is_url,uri = Uri.TryCreate(url, UriKind.Absolute)
        is_url && Array.exists ((=) uri.Scheme) allowed_schemas
        
    let clipboard_url_obs =
        (snd urls)
        |> AsyncRx.chooseAsync (fun () -> async {
            let! url = Js.InvokeAsync<string>("navigator.clipboard.readText").AsTask() |> Async.AwaitTask
            return if is_url url then Some url else None
        })
        
    member val URL = AsyncRx.merge files_obs clipboard_url_obs with get
    member _.UploadFile file = (fst files).OnNextAsync file
    member _.CopyUrlFromClipboard() = (fst urls).OnNextAsync()
        
            
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
