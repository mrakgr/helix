using Blazor.Diagrams.Core;
using Helix.Components;
using Helix.Nodes;

namespace Helix.Types;

public class HelixDiagram : Diagram
{

    public HelixDiagram() : base(DiagramOptions())
    {
        RegisterModelComponent<TextNode, TextComponent>();
        RegisterModelComponent<CompilationNode, CompilationComponent>();
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
                ScaleFactor = 1.3,
                Inverse = true, // Whether to inverse the direction of the zoom when using the wheel
            }
        };
    }
}