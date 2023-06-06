namespace Helix.Nodes

open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Threading.Tasks
open Blazor.Diagrams.Core
open Blazor.Diagrams.Core.Geometry
open Blazor.Diagrams.Core.Models
open Blazor.Diagrams.Core.Models.Base
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components.Forms
open Microsoft.AspNetCore.Components.Web
open Microsoft.JSInterop
open Thoth.Json.Net

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
    
module Images =
    type HelixUrlHandle(url : string, js : IJSRuntime) =
        member _.URL = url
        
        override this.Finalize() = js.InvokeVoidAsync("URL.revokeObjectURL", url) |> ignore
        
    type HelixUrlInfo =    
        {
            content_type : string
            guid : string
        }
        
    type HelixUrl =
        | HelixUrl of HelixUrlHandle * HelixUrlInfo
        
        static member Create (url : HelixUrlHandle) content_type = HelixUrl (url, { content_type = content_type; guid = $"image:{Guid.NewGuid().ToString()}" })
        
    let allowed_schemas = [|Uri.UriSchemeHttp; Uri.UriSchemeHttps; Uri.UriSchemeFile; Uri.UriSchemeFtp; Uri.UriSchemeFtps|]
    let is_url url =
        let is_url,uri = Uri.TryCreate(url, UriKind.Absolute)
        is_url && Array.exists ((=) uri.Scheme) allowed_schemas
        
    let create_object_url_from_stream (Js : IJSRuntime) (file : Stream) (content_type : string) = task {
        use dotnetImageStream = new DotNetStreamReference(file)
        let! url = Js.InvokeAsync<string>("createObjectURL_FromStream", dotnetImageStream, content_type)
        return HelixUrlHandle(url, Js)
    } 
        
    let upload_file' (Js : IJSRuntime) (file : Stream) (content_type : string) = task {
        let! url = create_object_url_from_stream Js file content_type
        return HelixUrl.Create url content_type
    }
        
    let upload_file (Js : IJSRuntime) (file : IBrowserFile) = upload_file' Js (file.OpenReadStream(file.Size)) file.ContentType 
    
    let copy_url_from_clipboard (Js : IJSRuntime) (http : HttpClient) = task {
        let! url = Js.InvokeAsync<string>("navigator.clipboard.readText")
        if is_url url then
            let! response = http.GetAsync(url)
            if response.IsSuccessStatusCode then
                let! file = response.Content.ReadAsStreamAsync()
                let! result = upload_file' Js file response.Content.Headers.ContentType.MediaType
                return Some result
            else return None
        else
            return None
    }
    
open Images
type ImageNode(p : Point, Js : IJSRuntime, Http : HttpClient) as node =
    inherit NodeModel(p)
    
    do node.AddPort(HelixPort(node, PortAlignment.Left, true)) |> ignore
    do node.AddPort(HelixPort(node, PortAlignment.Right, false)) |> ignore
    
    member val Src : HelixUrl = HelixUrl(HelixUrlHandle("images/sun-big.png",Js), { content_type="image/png"; guid="image:default"}) with get, set
    
    member _.UploadFile (file : IBrowserFile) = task { let! helix_url = upload_file Js file in node.Src <- helix_url }
    member _.CopyUrlFromClipboard() = task { let! helix_url = copy_url_from_clipboard Js Http in Option.iter (fun url -> node.Src <- url) helix_url }
    
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
        
module LocalForage =
    let get (js : IJSRuntime) (k : string) = js.InvokeAsync<_>("localforage.getItem", k)
    let set (js : IJSRuntime) (k : string) v = js.InvokeVoidAsync("localforage.setItem", k, v)
    
module StoreLoad =
    type StoredNodes =
        | S_TextNode of string
        | S_ImageNode of HelixUrlInfo
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
    
    let save_image_urls_to_indexeddb (js : IJSRuntime) (http : HttpClient) (urls : HelixUrl[]) =
        urls |> Array.map (fun (HelixUrl(handle,info)) -> task {
                let! data = http.GetAsync handle.URL
                let! text = data.EnsureSuccessStatusCode().Content.ReadAsByteArrayAsync()
                do! LocalForage.set js info.guid text
            }) |> Task.WhenAll
        
            
    let to_tuple (p : Point) = p.X, p.Y
    let store (diagram : Diagram) js http = task {
        let d_ports_from, d_ports_to = diagram_ids diagram.Nodes
        let nodes = diagram.Nodes |> Seq.toArray
        
        let store_task () =
            nodes |> Array.choose (function
                | :? ImageNode as n -> Some n.Src
                | _ -> None
                )
            |> save_image_urls_to_indexeddb js http
            
        let nodes =
            nodes |> Array.map (fun n ->
                let p = to_tuple n.Position
                let n = match n with
                        | :? TextNode as n -> S_TextNode n.Text
                        | :? ImageNode as n -> S_ImageNode (match n.Src with HelixUrl(_,info) -> info)
                        | :? CompilationNode as n -> S_CompilationNode
                        | :? DatabaseTestNode as n -> S_DatabaseTestNode
                        | n -> failwithf $"Type not supported: {n.GetType()}"
                n, p
                )
            |> fun (x : NodeT []) -> Encode.Auto.toString x
        
        let links =
            diagram.Links
            |> Seq.toArray
            |> Array.map (fun x ->
                d_ports_from[x.SourcePort], d_ports_from[x.TargetPort]
                )
            |> fun (x : LinkT []) -> Thoth.Json.Net.Encode.Auto.toString x
            
        let! _ = store_task ()
        do! LocalForage.set js "diagram_nodes" nodes
        do! LocalForage.set js "diagram_links" links
    }
       
    let load_images_from_indexeddb (js : IJSRuntime) (infos : HelixUrlInfo []) =
        infos |> Array.map (fun info -> task {
            let! (data : byte []) = LocalForage.get js info.guid
            let! url = js.InvokeAsync<string>("createObjectURL", data, info.content_type)
            return HelixUrl(HelixUrlHandle(url, js), info)
        }) |> Task.WhenAll
    
    let load (d : Diagram) (js : IJSRuntime) (http : HttpClient) = task {
        d.Nodes.Clear()
        d.Links.Clear()
        
        let decode nodes = 
            if nodes = null then [||]
            else Decode.Auto.unsafeFromString nodes
        let! nodes = LocalForage.get js "diagram_nodes"
        let! links = LocalForage.get js "diagram_links"
        let nodes : NodeT [] = decode nodes
        
        let! urls =
            nodes |> Array.choose (function
                | S_ImageNode info, _ -> Some info
                | _ -> None)
            |> load_images_from_indexeddb js
        
        let nodes =
            let mutable i = 0
            let get_i() = let x = i in i <- i+1; x
            nodes |> Array.map (function
                | S_TextNode s, p -> TextNode(Point p, Text=s) :> NodeModel
                | S_ImageNode info, p -> ImageNode(Point p, js, http, Src=urls[get_i()])
                | S_CompilationNode, p -> CompilationNode(Point p)
                | S_DatabaseTestNode, p -> DatabaseTestNode(Point p) 
                )
            
        let d_ports_from, d_ports_to = diagram_ids nodes
        
        let links =
            (decode links : LinkT [])
            |> Array.map (fun (source, target) ->
                LinkModel(d_ports_to[source],d_ports_to[target]) :> BaseLinkModel
                )
        d.Nodes.Add nodes; d.Links.Add links
    }
        
type UndoBufferActions =
    | U_NodeAdded of NodeModel
    | U_NodeRemoved of NodeModel
    | U_LinkAdded of BaseLinkModel
    | U_LinkRemoved of BaseLinkModel
    | U_NodesMoved of start: Dictionary<NodeModel,Point> * ``end``: Dictionary<NodeModel,Point>
        
open FSharp.Control.Reactive
type HelixDiagramBase(js : IJSRuntime, http : HttpClient, opts) as this =
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
            do! js.InvokeVoidAsync("registerUnloadEvent", DotNetObjectReference.Create(this));            
            is_loaded <- true

        do! StoreLoad.load this js http
    }
    
    member this.OnStore() = task {
        if is_loaded then
            do! StoreLoad.store this js http
    }
    
    [<JSInvokable>]
    member this.OnBeforeUnload() = this.OnStore()
    
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
