using Blazor.Diagrams.Core;
using Helix.Components;
using Helix.Nodes;
using Microsoft.JSInterop;

namespace Helix.Types;

public class HelixDiagram : Diagram
{
    private bool _isFirst = true;
    private IJSRuntime Js { get; }

    public HelixDiagram(IJSRuntime js) : base(DiagramOptions())
    {
        Js = js;
        RegisterModelComponent<TextNode, TextComponent>();
        RegisterModelComponent<CompilationNode, CompilationComponent>();
        RegisterModelComponent<ImageNode, ImageComponent>();
        RegisterModelComponent<DatabaseTestNode, DatabaseTestComponent>();
        
    }
    
    public async Task OnLoad()
    {
        if (_isFirst)
        {
            await Js.InvokeVoidAsync("registerUnloadEvent", DotNetObjectReference.Create(this));
            _isFirst = false;
        }
        StoreLoad.load(this,
            await Js.InvokeAsync<string>("localforage.getItem", "diagram_nodes"),
            await Js.InvokeAsync<string>("localforage.getItem", "diagram_links")
        );
    }

    public async Task OnStore()
    {
        if (!_isFirst)
        {
            var (nodes, links) = StoreLoad.store(this);
            await Js.InvokeVoidAsync("localforage.setItem", "diagram_nodes", nodes);
            await Js.InvokeVoidAsync("localforage.setItem", "diagram_links", links);
        }
    }
    
    [JSInvokable]
    public Task OnBeforeUnload()
    {
        return OnStore();
    }

    private static DiagramOptions DiagramOptions()
    {
        // I had to disable virtualization due to the zoom throwing exceptions.
        // It should be fixed in the next version.
        return new DiagramOptions
        {
            EnableVirtualization = false, // Work around for: https://github.com/Blazor-Diagrams/Blazor.Diagrams/issues/322
            DeleteKey = "Delete", // What key deletes the selected nodes/links
            DefaultNodeComponent = null, // Default component for nodes
            AllowMultiSelection = true, // Whether to allow multi selection using CTRL
            Links = new DiagramLinkOptions // Options related to links
            {
            },
            Zoom = new DiagramZoomOptions // Other
            {
                Minimum = 0.1, // Minimum zoom value
                Maximum = 10,
                ScaleFactor = 1.3,
                Inverse = true, // Whether to inverse the direction of the zoom when using the wheel
            }
        };
    }
}