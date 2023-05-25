using Blazor.Diagrams.Core.Geometry;
using Blazor.Diagrams.Core.Models;

namespace Helix.Nodes;

public class CompilationNode : NodeModel
{
    // This should be immutable, but nwm that for now.
    public required string Text { get; set; }
    
    public CompilationNode(Point p) : base(p)
    {
    }
}