using Blazor.Diagrams.Core.Geometry;
using Blazor.Diagrams.Core.Models;

namespace Helix.Nodes;

public class TextNode : NodeModel
{
    // This should be immutable, but nwm that for now.
    public required string Text { get; set; }

    public TextNode(Point p) : base(p)
    {
    }
}