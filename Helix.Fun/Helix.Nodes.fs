namespace Helix

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
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

module Tagger =
    let create() =
        let mutable i = 0
        fun () -> let x = i in i <- i+1; x

module LocalForage =
    let get (js : IJSRuntime) (k : string) = js.InvokeAsync<_>("localforage.getItem", k)
    let set (js : IJSRuntime) (k : string) v = js.InvokeVoidAsync("localforage.setItem", k, v)
    let remove (js : IJSRuntime) (k : string) = js.InvokeVoidAsync("localforage.removeItem", k)
    let clear (js : IJSRuntime) = js.InvokeVoidAsync("localforage.clear")
    let length (js : IJSRuntime) = js.InvokeAsync<int>("localforage.length")
    let keys (js : IJSRuntime) = js.InvokeAsync<string []>("localforage.keys")
   
module Data =
    [<AbstractClass>]
    type HelixPropagator() =
        abstract member PropagateChanges : NodeModel -> unit
        abstract member LoadPreviewNodes : NodeModel [] -> unit
    
    type [<MessagePack.MessagePackObject; Struct>] IndexDbLink<'t> = IndexDbLink of int
    
    [<MessagePack.MessagePackObject true>]
    type ArgsText = {
        text: IndexDbLink<string>
    }
    
    [<MessagePack.MessagePackObject true>]
    type ArgsImage = {
        image: IndexDbLink<byte []>
        content_type: string
    }
    
    [<MessagePack.MessagePackObject true>]
    type ArgsEmpty = {
        __placeholder: bool
    }
    
    [<MessagePack.MessagePackObject>]
    type HelixDbNode =
     | S_CssNode of ArgsText
     | S_TextNode of ArgsText
     | S_ImageNode of ArgsImage
     | S_CompilationNode of ArgsEmpty
     | S_DatabaseTestNode of ArgsEmpty
     | S_PreviewNode of ArgsEmpty
     
     type HelixDbLink<'t> =
         abstract member DbLink : 't IndexDbLink
         abstract member Get : 't option Task
         abstract member Set : 't -> Task
         
    let id_to_key (x : int) = string x
    let key_to_id (x : string) = Int32.Parse x
    type MediaPool() =
        let rng = Random()
        let set = HashSet()
    
        member m.GetLink() : _ IndexDbLink =
             let id = rng.Next(Int32.MinValue,Int32.MaxValue)
             if set.Add id then IndexDbLink id
             else m.GetLink()
             
        member m.AddLink (IndexDbLink x) = set.Add x |> ignore
        
        /// CLears the pool before adding the links.
        member m.AddInitialLinks used =
            set.Clear()
            for id in used do m.AddLink (IndexDbLink id)
             
        /// Removes unused keys from the IndexedDb database.
        member m.CleanUp (js : IJSRuntime) = task {
            let! indexdb_keys = LocalForage.keys js
            for k in indexdb_keys do
                match Int32.TryParse k with
                | true,v when set.Contains v -> ()
                | _ -> do! LocalForage.remove js k
        }
        
open Data

module Link =
    let get js (IndexDbLink id) = task {
        match! LocalForage.get js (id_to_key id) with
        | null -> return None
        | x -> return Some x
        }
    
    let set js (IndexDbLink id) v = (LocalForage.set js (id_to_key id) v).AsTask()
    
    let create js (db_link : 'a IndexDbLink) =
        {new HelixDbLink<'a> with
            member this.DbLink = db_link
            member this.Get = get js db_link
            member this.Set v = set js db_link v
            }
    
module Nodes =
    type HelixPortType =
        | Data
        
    type HelixPort(parent : NodeModel, alignment, isInput : bool, port_type) =
        inherit PortModel(parent, alignment, null, null)
        
        member val IsInput = isInput with get
        member val PortType = port_type with get

        override this.CanAttachTo(port) =
            // This offset is because `this` already has a link by the time this function gets called.
            let is_input_port_empty (a : HelixPort) by = a.IsInput && a.Links.Count + by = 0 
            match port with
            | :? HelixPort as cp ->
                // Checks for same-node/port attachements.
                base.CanAttachTo(port)
                // The types need to be the same.
                && this.PortType = cp.PortType
                // Makes sure only input <-> output pairs are valid.
                && this.IsInput <> cp.IsInput
                // We also need to make sure that an input port only takes a single input.
                && (is_input_port_empty this -1 || is_input_port_empty cp 0)
            | _ -> false
            
    module Topological =
        exception CycleException of NodeModel
        
        let inline memoize (dict : Dictionary<_, _>) f x =
            match dict.TryGetValue(x) with
            | true, v -> v
            | _ -> let y = f x in dict.Add(x,y); y
            
        let template cond (start_node : NodeModel) =
            let d = Dictionary()
            let ordered_nodes = ResizeArray()
            let visited_nodes = HashSet()
            let rec dfs node =
                memoize d (fun (node : NodeModel) ->
                    if visited_nodes.Add node = false then raise (CycleException node)
                    for port in node.Ports do
                        match port with
                        | :? HelixPort as port when cond port ->
                            for link in port.Links do
                                let target = if link.TargetNode <> node then link.TargetNode else link.SourceNode
                                if target <> null then dfs target
                        | _ -> ()
                        
                    // Also adding the nodes really needs to be here.
                    // It won't give the correct results if you put it in first place.
                    ordered_nodes.Add node
                    ) node

            dfs start_node
            ordered_nodes.ToArray()
            
        /// Returns the nodes in their topological order from the starting node, going through the input ports.
        let path_from_input_ports start_node = template (fun n -> n.IsInput) start_node
        /// Returns the nodes in their topological order from the starting node, going through the output ports.
        let path_from_output_ports start_node = template (fun n -> not n.IsInput) start_node
        
        
    [<AbstractClass>]
    type HelixNode(p : Point) =
        inherit NodeModel(p)
        abstract member ToDbNode : HelixDbNode
        abstract member Initialize :  unit -> Task
        
        
        
    let init (x : #HelixNode) = task {
        do! x.Initialize()
        return x :> NodeModel
    }
   
    [<AbstractClass>]
    type TextNodeBase(p : Point, db : string HelixDbLink, prop : HelixPropagator) =
        inherit HelixNode(p)
        
        member val Text = "" with get, set
        member this.OnChange (text : string) = task {
            this.Text <- text
            prop.PropagateChanges this
            return! db.Set(text)
        }
        
        override this.Initialize() = task {
            let! text = db.Get
            Option.iter (fun text -> this.Text <- text) text
        }
        
        
    type TextNode(p, db, prop) as node =
        inherit TextNodeBase(p,db,prop)
        
        do node.AddPort(HelixPort(node, PortAlignment.Left, true, Data)) |> ignore
        do node.AddPort(HelixPort(node, PortAlignment.Right, false, Data)) |> ignore
        do node.AddPort(HelixPort(node, PortAlignment.Bottom, false, Data)) |> ignore
        
        override this.ToDbNode = S_TextNode({text = db.DbLink})
        
        static member Create p link prop js = TextNode(p,Link.create js link,prop) |> init
        
    type CssNode(p, db, prop) as node =
        inherit TextNodeBase(p,db,prop)
        
        do node.AddPort(HelixPort(node, PortAlignment.Left, true, Data)) |> ignore
        do node.AddPort(HelixPort(node, PortAlignment.Right, false, Data)) |> ignore
        
        override this.ToDbNode = S_CssNode({text = db.DbLink})
        
        static member Create p link prop js = CssNode(p,Link.create js link,prop) |> init
        
    module Images =
        type HelixUrlHandle(url : string, content_type : string, js : IJSRuntime) =
            member _.URL = url
            member _.ContentType = content_type
            
            override this.Finalize() = js.InvokeVoidAsync("URL.revokeObjectURL", url) |> ignore
            
        type HelixUrl =
            | HelixDirectUrl of string * content_type: string
            | HelixBlobUrl of HelixUrlHandle
            
            member this.Url =
                match this with
                | HelixDirectUrl(s, _) -> s
                | HelixBlobUrl helixUrlHandle -> helixUrlHandle.URL
            
            member this.ContentType =
                match this with
                | HelixDirectUrl(_, contentType) -> contentType
                | HelixBlobUrl x -> x.ContentType
                
        let stream_to_byte_array (file : Stream) = task {
            let len = Convert.ToInt32 file.Length
            let ar = Array.zeroCreate len
            let! _bytes_read = file.ReadAsync(ar,0,len)
            return ar
        }
        
        let create_object_url (Js : IJSRuntime) (file : byte []) (content_type : string) = task {
            let! url = Js.InvokeAsync<string>("createObjectURL", file, content_type)
            return HelixUrlHandle(url, content_type, Js)
        }
        
        let create_object_url_from_stream (Js : IJSRuntime) (file : Stream) (content_type : string) = task {
            let! ar = stream_to_byte_array file
            let! url = create_object_url Js ar content_type
            return url, ar
        }
            
        let upload_file' (Js : IJSRuntime) (file : Stream) (content_type : string) = task {
            let! url, ar = create_object_url_from_stream Js file content_type
            return HelixBlobUrl url, ar
        }
        let upload_file (Js : IJSRuntime) (file : IBrowserFile) = upload_file' Js (file.OpenReadStream(file.Size)) file.ContentType
        
        let allowed_schemas = [|Uri.UriSchemeHttp; Uri.UriSchemeHttps; Uri.UriSchemeFile; Uri.UriSchemeFtp; Uri.UriSchemeFtps|]
        let is_url (url : string) =
            let url =
                let prefix = "blob:"
                if url.StartsWith prefix then url.Substring prefix.Length else url
            let is_url,uri = Uri.TryCreate(url, UriKind.Absolute)
            is_url && Array.exists ((=) uri.Scheme) allowed_schemas
            
        let content_types = [|"image/gif"; "image/jpeg"; "image/png"; "image/svg+xml"; "image/webp"|]
        let base64_image_prefix x = $"data:%s{x};base64,"
        let try_base64_image (url : string) = content_types |> Array.tryFind (base64_image_prefix >> url.StartsWith)
        
        let copy_url_from_clipboard (Js : IJSRuntime) (http : HttpClient) = task {
            let! url = Js.InvokeAsync<string>("navigator.clipboard.readText")
            if is_url url then
                let! response = http.GetAsync(url)
                if response.IsSuccessStatusCode then
                    let! file = response.Content.ReadAsByteArrayAsync()
                    let content_type = response.Content.Headers.ContentType.MediaType
                    if Array.exists ((=) content_type) content_types then
                        let! result = create_object_url Js file content_type
                        return Some (HelixBlobUrl result, file)
                    else
                        return None
                else return None
            else
                match! Js.InvokeAsync<obj []>("getImageFromClipboard") with
                | [|url; content_type|] ->
                    let url, content_type = url.ToString(), content_type.ToString()
                    let! ar = http.GetByteArrayAsync url
                    return Some (HelixBlobUrl(HelixUrlHandle(url,content_type,Js)),ar)
                | _ -> return None
        }
        
    open Images
    type ImageNode(p : Point, db : byte [] HelixDbLink, content_type, js,prop : HelixPropagator) as node =
        inherit HelixNode(p)
        
        do node.AddPort(HelixPort(node, PortAlignment.Left, true, Data)) |> ignore
        do node.AddPort(HelixPort(node, PortAlignment.Right, false, Data)) |> ignore
        do node.AddPort(HelixPort(node, PortAlignment.Bottom, false, Data)) |> ignore
        
        member val Src : HelixUrl = HelixDirectUrl("images/sun-big.png", "image/png") with get, set
        
        member this.GetExtension =
            match this.Src with
            | HelixBlobUrl helixUrlHandle -> helixUrlHandle.ContentType
            | HelixDirectUrl(_, content_type) -> content_type
            |> fun x -> x.Split('/')[1]
            
        member _.GetImage = db.Get
        
        member _.UploadFile js _http (file : IBrowserFile) : Task = task { let! helix_url = upload_file js file in do! node.OnChange helix_url }
        member _.CopyUrlFromClipboard js http : Task = task {
            let! helix_url = copy_url_from_clipboard js http
            match helix_url with
            | Some helix_url -> do! node.OnChange helix_url
            | None -> ()
        }
        
        member this.OnChange (x, ar) : Task = task {
            this.Src <- x
            prop.PropagateChanges this
            do! db.Set ar
        }
        
        override this.Initialize () = task {
            match! db.Get with
            | Some ar ->
                let! url = create_object_url js ar content_type
                do! this.OnChange (HelixBlobUrl url, ar)
            | None ->
                ()
        }
        
        override this.ToDbNode = S_ImageNode {image = db.DbLink; content_type=this.Src.ContentType}
        
        static member Create p link content_type prop js = ImageNode(p,Link.create js link,content_type,js,prop) |> init
        static member Default p link prop js = task {return ImageNode(p,Link.create js link,"image/png",js,prop) :> NodeModel}
        
    type PreviewNode(p) as node =
        inherit HelixNode(p)
        
        do node.AddPort(HelixPort(node, PortAlignment.Top, true, Data)) |> ignore
        
        member val Body = MarkupString "<b>qwe</b>" with get,set

        override this.Initialize() = Task.CompletedTask
        override this.ToDbNode = S_PreviewNode {__placeholder=true}
            
        static member Create p = PreviewNode(p) |> init
            
            
    type DatabaseTestNode(p : Point) as node =
        inherit HelixNode(p)
        
        do node.AddPort(HelixPort(node, PortAlignment.Top, true, Data)) |> ignore
    
        override this.ToDbNode = S_DatabaseTestNode {__placeholder=true}
        override this.Initialize() = Task.CompletedTask
        
        static member Create p = DatabaseTestNode p |> init
        
    type CompilationNode(p : Point) as node =
        inherit HelixNode(p)
        
        do node.AddPort(HelixPort(node, PortAlignment.Left, true, Data)) |> ignore // It doesn't have an output port.
        
        member val Body = MarkupString "<b>qwe</b>" with get,set
        
        override this.ToDbNode = S_CompilationNode {__placeholder=true}
        override this.Initialize() = Task.CompletedTask
        
        static member Create p = CompilationNode p |> init
        
open Nodes

module Utils =
    let add_node (d : Diagram, node_constructor : Func<Point, NodeModel Task>) (ev : MouseEventArgs) = task {
        let to_canvas_point (x,y) = d.GetRelativeMousePoint(x,y)
        let! node = node_constructor.Invoke(to_canvas_point(ev.ClientX, ev.ClientY))
        d.Nodes.Add(node)
    }
    
    let port_styles (_port : PortModel) = ""
   
module StoreLoad =
    module LocalStorage =
        let get (js : IJSRuntime) (k : string) = js.InvokeAsync<_>("localStorage.getItem", k)
        let set (js : IJSRuntime) (k : string) v = js.InvokeVoidAsync("localStorage.setItem", k, v)
    
    open MessagePack
    type [<MessagePackObject true>] NodeT = { node : HelixDbNode; point : double * double }
    type LinkTKey = int * PortAlignment * int
    type [<MessagePackObject true>] LinkT = { source : LinkTKey; target : LinkTKey }
    
    let local_storage_to js (nodes : NodeT []) (links : LinkT []) = task {
        do! LocalStorage.set js "diagram_nodes" (Encode.Auto.toString nodes)
        do! LocalStorage.set js "diagram_links" (Encode.Auto.toString links)
    }
    
    let local_storage_from js : (NodeT [] * LinkT []) Task = task {
        let decode nodes = 
            if nodes = null then [||]
            else Decode.Auto.unsafeFromString nodes
        let! nodes = LocalStorage.get js "diagram_nodes"
        let! links = LocalStorage.get js "diagram_links"
        return decode nodes, decode links
    }
    
    let diagram_ids (nodes : #NodeModel []) =
        let d_ports_from = Dictionary(HashIdentity.Reference)
        let d_ports_to = Dictionary()
        
        nodes |> Array.iteri (fun i x ->
            x.Ports
            |> Seq.groupBy (fun x -> x.Alignment)
            |> Seq.iter (fun (align, ports) ->
                ports |> Seq.iteri (fun i' port ->
                    let key = i, align, i' 
                    d_ports_from.Add(port, key); d_ports_to.Add(key, port)
                    )
                )
            )
            
        d_ports_from, d_ports_to
        
    let to_db_node (n : NodeModel) = {
        node = (n :?> HelixNode).ToDbNode
        point = let n = n.Position in n.X, n.Y 
    }
        
    let store_nodes_links (nodes : NodeModel []) (links : BaseLinkModel []) =
        let d_ports_from, _ = diagram_ids nodes
        
        let nodes = nodes |> Array.map to_db_node
        let links = links |> Array.map (fun x -> {source = d_ports_from[x.SourcePort]; target = d_ports_from[x.TargetPort]})
        nodes, links
        
    
    let store' (diagram : Diagram) : NodeT [] * LinkT [] = store_nodes_links (Seq.toArray diagram.Nodes) (Seq.toArray diagram.Links)
    
    let load_nodes_links prop js (nodes : NodeT []) (links : LinkT []) = task {
        let! nodes =
            nodes |> Array.map (fun x ->
                let p = Point x.point
                match x.node with
                | S_CssNode s -> CssNode.Create p s.text prop js
                | S_TextNode s -> TextNode.Create p s.text prop js
                | S_ImageNode x -> ImageNode.Create p x.image x.content_type prop js
                | S_PreviewNode {__placeholder=_} -> PreviewNode.Create p
                | S_CompilationNode {__placeholder=_} -> CompilationNode.Create p
                | S_DatabaseTestNode {__placeholder=_} -> DatabaseTestNode.Create p
                ) |> Task.WhenAll
        let _, d_ports_to = diagram_ids nodes
        let links =
            links |> Array.map (fun x ->
                LinkModel(d_ports_to[x.source],d_ports_to[x.target]) :> BaseLinkModel
                )
            
        return nodes, links
    }
    
    let media_pool_initialize js (m : MediaPool) (nodes : NodeT []) =
        nodes
        |> Array.choose (fun n ->
            match n.node with
            | S_CssNode {text=IndexDbLink i}
            | S_TextNode {text=IndexDbLink i} | S_ImageNode {image=IndexDbLink i} -> Some i
            | S_CompilationNode _ | S_DatabaseTestNode _ | S_PreviewNode _ -> None
            )
        |> m.AddInitialLinks
        
        m.CleanUp js
        
    let load' prop (d : Diagram) (js : IJSRuntime) (m : MediaPool) nodes links = task {
        d.Nodes.Clear(); d.Links.Clear()
        
        do! media_pool_initialize js m nodes
        let! nodes, links = load_nodes_links prop js nodes links
                    
        d.Nodes.Add nodes; d.Links.Add links
        prop.LoadPreviewNodes nodes
    }
    
    let load_on_visibility_change prop (d : Diagram) (js : IJSRuntime) (m : MediaPool) = task {
        let! nodes, links = local_storage_from js
        do! load' prop d js m nodes links
    }
        
    let store_on_visibility_change (diagram : Diagram) js _http = task {
        let nodes, links = store' diagram
        do! local_storage_to js nodes links
    }
    
    open MessagePack.Resolvers
    open MessagePack.FSharp
       
    type [<MessagePackObject true>] HelixDiagramFile = {
        nodes : NodeT []
        links : LinkT []
        images : (int * byte []) []
        text : (int * string) []
    }
    
    let store_mp js diagram = task {
        let images, text = ResizeArray(), ResizeArray()
        let nodes, links = store' diagram
        for n in nodes do
            match n.node with
            | S_CssNode {text=IndexDbLink id as x} | S_TextNode {text=IndexDbLink id as x} ->
                let! x = Link.get js x
                Option.iter (fun x -> text.Add(id,x)) x
            | S_ImageNode {image=IndexDbLink id as x} ->
                let! x = Link.get js x
                Option.iter (fun x -> images.Add(id,x)) x
            | S_CompilationNode _ | S_DatabaseTestNode _ | S_PreviewNode _ -> ()
            
        return {
            nodes = nodes
            links = links
            images = images.ToArray()
            text = text.ToArray()
        }
    }
    
    let load_mp prop js (m : MediaPool) diagram (n : HelixDiagramFile) = task {
        for id,ar in n.images do
            do! LocalForage.set js (id_to_key id) ar
        for id,ar in n.text do
            do! LocalForage.set js (id_to_key id) ar
            
        do! load' prop diagram js m n.nodes n.links
    }
    
    module private MessagePackUtils =
        let resolver = CompositeResolver.Create(FSharpResolver.Instance, StandardResolver.Instance)
        let options = MessagePackSerializerOptions.Standard.WithResolver(resolver)
        
        let serialize (value: HelixDiagramFile) = MessagePackSerializer.Serialize(value, options)
        let deserialize (value: byte []) = MessagePackSerializer.Deserialize<HelixDiagramFile>(value, options)
        
    open MessagePackUtils
    let on_database_download js diagram = task {
        let! mp = store_mp js diagram
        do! js.InvokeVoidAsync("downloadFile","diagram.helixdb", serialize mp)
    }
    
    let database_upload prop js m diagram (file : IBrowserFile) = task {
        let! ar = Images.stream_to_byte_array (file.OpenReadStream(file.Size) )
        do! ar |> deserialize |> load_mp prop js m diagram
    }

type UndoBufferActions =
    | U_NodeAdded of NodeModel
    | U_NodeRemoved of NodeModel
    | U_LinkAdded of BaseLinkModel
    | U_LinkRemoved of BaseLinkModel
    | U_NodesMoved of start: Dictionary<NodeModel,Point> * ``end``: Dictionary<NodeModel,Point>
        
open FSharp.Control.Reactive
type HelixDiagramBase(prop, js : IJSRuntime, http : HttpClient, m : MediaPool, opts) as this =
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
            | U_NodesMoved(start, _) -> nodes_moved start
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
            | U_NodesMoved(_, ``end``) -> nodes_moved ``end``
            is_handler_active <- true
            
    member this.OnLoad() = task {
        if not is_loaded then
            do! js.InvokeVoidAsync("registerUnloadEvent", DotNetObjectReference.Create(this));            
            is_loaded <- true

        do! StoreLoad.load_on_visibility_change prop this js m
    }
    
    member this.OnStore() = task {
        if is_loaded then
            do! StoreLoad.store_on_visibility_change this js http
    }
    
    [<JSInvokable>]
    member this.OnVisibilityChangeHidden() = this.OnStore()
    
module Compilation =
    module Html =
        open Giraffe.ViewEngine
        let css (text : string) = style [] [rawText text] 
        let img (src : string) = img [_class "helix-image"; _src src]
        let p (text : string) = p [_class "helix-text"] [encodedText text]
        let master (order : XmlNode []) =
            html [] [
                head [] []
                body [] (Array.toList order)
            ]
        
        let as_text x = RenderView.AsString.htmlDocument x
        
    type CompHTMLNodes =
        | C_Css of text: string
        | C_Text of text: string
        | C_Image of src: string
        
    type CompDataNodes =
        | D_Image of src: string * byte [] Task
        
    let image_path' i extension = $"images/%s{i}.%s{extension}"
    let image_path i (n : ImageNode) = image_path' $"{i:D4}" n.GetExtension
        
    let compile_data_nodes (http : HttpClient) (order : NodeModel []) =
        let i = Tagger.create()
        order |> Array.choose (function
            | :? ImageNode as n ->
                D_Image (image_path (i()) n, task {
                    match! n.GetImage with
                    | Some x -> return x
                    | None -> return! http.GetByteArrayAsync n.Src.Url
                }) |> Some
            | _ ->
                None
            )
        
    let compile_html_nodes is_download (order : NodeModel []) =
        let i = Tagger.create()
        order |> Array.choose (function
            | :? CssNode as n -> Some (C_Css(n.Text))
            | :? TextNode as n -> Some (C_Text(n.Text))
            | :? ImageNode as n -> Some (C_Image (if is_download then image_path (i()) n else n.Src.Url))
            | _ -> None
            )
        
    let compile_html (order : CompHTMLNodes []) =
        Array.map (function
            | C_Css s -> Html.css s
            | C_Text s -> Html.p s
            | C_Image s -> Html.img s
            ) order
        |> Html.master
        |> Html.as_text
        
    let compile_html_zip (order : CompHTMLNodes []) (archive : ZipArchive) = task {
        let result = compile_html order
        let entry = archive.CreateEntry($"index.html")
        use writer = new StreamWriter(entry.Open())
        do! writer.WriteAsync(result)
        }
        
    let compile_data_zip (order : CompDataNodes []) (archive : ZipArchive) = task {
        for i=0 to order.Length-1 do
            match order[i] with
            | D_Image(src, data) ->
                let entry = archive.CreateEntry(src)
                let! data = data
                use writer = new BinaryWriter(entry.Open())
                writer.Write(data)
        }
    
    let use_archive f = task {
        use stream = new MemoryStream()
        if true then
            use archive = new ZipArchive(stream,ZipArchiveMode.Update)
            do! f archive
        return stream.ToArray()
    }
    
    let compile_preview_as_zip http (start_node : CompilationNode) = use_archive <| fun archive -> task {
        let order = Topological.path_from_input_ports start_node
        do! compile_data_zip (compile_data_nodes http order) archive
        do! compile_html_zip (compile_html_nodes true order) archive
    }
    
    let compilation_preview_last start_node =
        let css, rest =
            Topological.path_from_input_ports start_node
            |> compile_html_nodes false
            |> Array.partition (function C_Css _ -> true | _ -> false)
        match Array.tryLast rest with
        | Some x -> Array.append css [|x|]
        | None -> css
        |> compile_html
    
    let compilation_preview' start_node =
        let order = Topological.path_from_input_ports start_node
        compile_html (compile_html_nodes false order)
        
    let compilation_preview (start_node : CompilationNode) =
        start_node.Body <- MarkupString(compilation_preview' start_node)

    let compile_raw (start_node : CompilationNode) = use_archive <| fun archive -> task {
        let order = Topological.path_from_input_ports start_node
        for i=0 to order.Length-1 do
            match order[i] with
            | :? CssNode as n ->
                let entry = archive.CreateEntry($"{i:D4}.css")
                use writer = new StreamWriter(entry.Open())
                do! writer.WriteAsync(n.Text)
            | :? TextNode as n ->
                let entry = archive.CreateEntry($"{i:D4}.txt")
                use writer = new StreamWriter(entry.Open())
                do! writer.WriteAsync(n.Text)
            | :? ImageNode as n ->
                match! n.GetImage with
                | Some image ->
                    let entry = archive.CreateEntry($"{i:D4}.%s{n.GetExtension}")
                    use writer = new BinaryWriter(entry.Open())
                    writer.Write(image)
                | None ->
                    ()
            | _ ->
                ()
    }
    
        
    let download_raw (js : IJSRuntime) (start_node : CompilationNode) = task {
        let! zip_file_array = compile_raw start_node 
        do! js.InvokeVoidAsync("downloadFile","diagram_raw.zip",zip_file_array)
    }
    
    let download_preview_as_zip http (js : IJSRuntime) (start_node : CompilationNode) = task {
        let! zip_file_array = compile_preview_as_zip http start_node
        do! js.InvokeVoidAsync("downloadFile","diagram_html.zip",zip_file_array)
    }
    
type Propagator() =
    inherit HelixPropagator()
    
    let update (n : NodeModel) = 
         match n with
         | :? PreviewNode as n ->
             n.Body <- MarkupString (Compilation.compilation_preview_last n)
             n.RefreshAll()
         | :? CompilationNode as n ->
             n.Body <- MarkupString (Compilation.compilation_preview' n)
             n.RefreshAll()
         | _ -> ()
    
    override this.PropagateChanges n =
         for n in Topological.path_from_output_ports n do update n

    override this.LoadPreviewNodes nodes =
        for n in nodes do update n