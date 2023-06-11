namespace Helix

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

module LocalForage =
    let get (js : IJSRuntime) (k : string) = js.InvokeAsync<_>("localforage.getItem", k)
    let set (js : IJSRuntime) (k : string) v = js.InvokeVoidAsync("localforage.setItem", k, v)
    let remove (js : IJSRuntime) (k : string) = js.InvokeVoidAsync("localforage.removeItem", k)
    let clear (js : IJSRuntime) = js.InvokeVoidAsync("localforage.clear")
    let length (js : IJSRuntime) = js.InvokeAsync<int>("localforage.length")
    let keys (js : IJSRuntime) = js.InvokeAsync<string []>("localforage.keys")
    
module Data =
    type [<MessagePack.MessagePackObject; Struct>] IndexDbLink<'t> = IndexDbLink of int
    
    [<MessagePack.MessagePackObject true>]
    type TextNodeArgs = {
        db: IndexDbLink<string>
        style: string
    }
    
    [<MessagePack.MessagePackObject true>]
    type ImageNodeArgs = {
        db: IndexDbLink<byte []>
        content_type: string
        style: string
    }
    
    [<MessagePack.MessagePackObject true>]
    type CompilationNodeArgs = {
        inline_styles : bool
    }
    
    [<MessagePack.MessagePackObject true>]
    type DatabaseTestNode = {
        __placeholder: bool
    }
    
    [<MessagePack.MessagePackObject>]
    type HelixDbNode =
     | S_TextNode of TextNodeArgs
     | S_ImageNode of ImageNodeArgs
     | S_CompilationNode of CompilationNodeArgs
     | S_DatabaseTestNode of DatabaseTestNode
     
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
    type HelixPort(parent : NodeModel, alignment, isInput : bool) =
        inherit PortModel(parent, alignment, null, null)
        
        member val IsInput = isInput with get

        override this.CanAttachTo(port) =
            // This offset is because `this` already has a link by the time this function gets called.
            let _is_input_port_empty (a : HelixPort) by = a.IsInput && a.Links.Count + by = 0 
            match port with
            | :? HelixPort as cp ->
                // Checks for same-node/port attachements.
                base.CanAttachTo(port)
                // Makes sure only input - output pairs are valid.
                && this.IsInput <> cp.IsInput
                // We also need to make sure that an input port only takes a single input.
                // && (is_input_port_empty this -1 || is_input_port_empty cp 0)
            | _ -> false
            
    [<AbstractClass>]
    type HelixNode(p : Point) =
        inherit NodeModel(p)
        abstract member ToDbNode : HelixDbNode
        abstract member Initialize :  unit -> Task
        
    let init (x : #HelixNode) = task {
        do! x.Initialize()
        return x :> NodeModel
    }
    
    type TextNode(p : Point, db : string HelixDbLink) as node =
        inherit HelixNode(p)
        
        do node.AddPort(HelixPort(node, PortAlignment.Left, true)) |> ignore
        do node.AddPort(HelixPort(node, PortAlignment.Right, false)) |> ignore
        
        member val Text = "" with get, set
        member this.OnChange (text : string) = task {
            this.Text <- text
            return! db.Set(text)
        }
        
        override this.ToDbNode = S_TextNode({db = db.DbLink; style = ""})
        override this.Initialize() = task {
            let! text = db.Get
            Option.iter (fun text -> this.Text <- text) text
        }
        
        static member Create p link js = TextNode(p,Link.create js link) |> init
        
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
                    let! response = http.GetAsync(url)
                    let! ar = response.EnsureSuccessStatusCode().Content.ReadAsByteArrayAsync()
                    return Some (HelixBlobUrl(HelixUrlHandle(url,content_type,Js)),ar)
                | _ -> return None
        }
        
    open Images
    type ImageNode(p : Point, db : byte [] HelixDbLink, content_type, js) as node =
        inherit HelixNode(p)
        
        do node.AddPort(HelixPort(node, PortAlignment.Left, true)) |> ignore
        do node.AddPort(HelixPort(node, PortAlignment.Right, false)) |> ignore
        
        member val Src : HelixUrl = HelixDirectUrl("images/sun-big.png", "image/png") with get, set
        
        member _.UploadFile js _http (file : IBrowserFile) : Task = task { let! helix_url = upload_file js file in do! node.OnChange helix_url }
        member _.CopyUrlFromClipboard js http : Task = task {
            let! helix_url = copy_url_from_clipboard js http
            match helix_url with
            | Some helix_url -> do! node.OnChange helix_url
            | None -> ()
        }
        
        member this.OnChange (x, ar) : Task = task {
            this.Src <- x
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
        
        override this.ToDbNode = S_ImageNode {db = db.DbLink; content_type=this.Src.ContentType; style=""}
        
        static member Create p link content_type js = ImageNode(p,Link.create js link,content_type,js) |> init
        static member Default p link js = task {return ImageNode(p,Link.create js link,"image/png",js) :> NodeModel}
            
    type DatabaseTestNode(p : Point) =
        inherit HelixNode(p)
    
        override this.ToDbNode = S_DatabaseTestNode {__placeholder=true}
        override this.Initialize() = Task.CompletedTask
        
        static member Create p = DatabaseTestNode p |> init
        
    type CompilationNode(p : Point) as node =
        inherit HelixNode(p)
        
        do node.AddPort(HelixPort(node, PortAlignment.Left, true)) |> ignore // It doesn't have an output port.
        
        member val ChildComponent : RenderFragment IEnumerable = [||] with get, set
        
        override this.ToDbNode = S_CompilationNode {inline_styles=true}
        override this.Initialize() = Task.CompletedTask
        
        static member Create p = CompilationNode p |> init
        
open Nodes

module Utils =
    let add_node (d : Diagram, node_constructor : Func<Point, NodeModel Task>) (ev : MouseEventArgs) = task {
        let to_canvas_point (x,y) = d.GetRelativeMousePoint(x,y)
        let! node = node_constructor.Invoke(to_canvas_point(ev.ClientX, ev.ClientY))
        d.Nodes.Add(node)
    }
    
module StoreLoad =
    module LocalStorage =
        let get (js : IJSRuntime) (k : string) = js.InvokeAsync<_>("localStorage.getItem", k)
        let set (js : IJSRuntime) (k : string) v = js.InvokeVoidAsync("localStorage.setItem", k, v)
    
    open MessagePack
    type [<MessagePackObject true>] NodeT = { node : HelixDbNode; point : double * double }
    type [<MessagePackObject true>] LinkT = { source : int; target : int }
    
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
        
        nodes |> Array.iter (fun x ->
            x.Ports |> Seq.iter (fun x ->
                let i = d_ports_from.Count
                d_ports_from.Add(x,i) // Maps the ports to unique ids.
                d_ports_to.Add(i,x) // Maps the unique ids to ports
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
    
    let load_nodes_links js (nodes : NodeT []) (links : LinkT []) = task {
        let! nodes =
            nodes |> Array.map (fun x ->
                let p = Point x.point
                match x.node with
                | S_TextNode s -> TextNode.Create p s.db js
                | S_ImageNode x -> ImageNode.Create p x.db x.content_type js
                | S_CompilationNode x -> CompilationNode.Create p
                | S_DatabaseTestNode x -> DatabaseTestNode.Create p
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
            | S_ImageNode {db=IndexDbLink i} | S_TextNode {db=IndexDbLink i} -> Some i
            | S_CompilationNode _ | S_DatabaseTestNode _ -> None
            )
        |> m.AddInitialLinks
        
        m.CleanUp js
        
    let load' (d : Diagram) (js : IJSRuntime) (m : MediaPool) nodes links = task {
        d.Nodes.Clear(); d.Links.Clear()
        
        do! media_pool_initialize js m nodes
        let! nodes, links = load_nodes_links js nodes links
                    
        d.Nodes.Add nodes; d.Links.Add links
    }
    
    let load_on_visibility_change (d : Diagram) (js : IJSRuntime) (m : MediaPool) = task {
        let! nodes, links = local_storage_from js
        do! load' d js m nodes links
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
            | S_ImageNode {db=IndexDbLink id as x} ->
                let! x = Link.get js x
                Option.iter (fun x -> images.Add(id,x)) x
            | S_TextNode {db=IndexDbLink id as x} ->
                let! x = Link.get js x
                Option.iter (fun x -> text.Add(id,x)) x
            | S_CompilationNode _ | S_DatabaseTestNode _ -> ()
            
        return {
            nodes = nodes
            links = links
            images = images.ToArray()
            text = text.ToArray()
        }
    }
    
    let load_mp js (m : MediaPool) diagram (n : HelixDiagramFile) = task {
        for id,ar in n.images do
            do! LocalForage.set js (id_to_key id) ar
        for id,ar in n.text do
            do! LocalForage.set js (id_to_key id) ar
            
        do! load' diagram js m n.nodes n.links
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
    
    let database_upload js m diagram (file : IBrowserFile) = task {
        let! ar = Images.stream_to_byte_array (file.OpenReadStream(file.Size) )
        do! ar |> deserialize |> load_mp js m diagram
    }

type UndoBufferActions =
    | U_NodeAdded of NodeModel
    | U_NodeRemoved of NodeModel
    | U_LinkAdded of BaseLinkModel
    | U_LinkRemoved of BaseLinkModel
    | U_NodesMoved of start: Dictionary<NodeModel,Point> * ``end``: Dictionary<NodeModel,Point>
        
open FSharp.Control.Reactive
type HelixDiagramBase(js : IJSRuntime, http : HttpClient, m : MediaPool, opts) as this =
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

        do! StoreLoad.load_on_visibility_change this js m
    }
    
    member this.OnStore() = task {
        if is_loaded then
            do! StoreLoad.store_on_visibility_change this js http
    }
    
    [<JSInvokable>]
    member this.OnVisibilityChangeHidden() = this.OnStore()
    
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
