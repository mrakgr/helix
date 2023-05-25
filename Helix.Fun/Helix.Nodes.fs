namespace Helix.Nodes

open Blazor.Diagrams.Core
open Blazor.Diagrams.Core.Geometry
open Blazor.Diagrams.Core.Models

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
            && (is_input_port_empty this -1 || is_input_port_empty cp 0)
        | _ -> false

module Compilation =
    let compile (diagram : Diagram) (start_node : CompilationNode) =
        
        ()