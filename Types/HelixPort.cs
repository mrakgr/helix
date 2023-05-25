using Blazor.Diagrams.Core.Models;

namespace Helix.Nodes;

public class HelixPort : PortModel
{
    public HelixPort(NodeModel parent, PortAlignment alignment, bool isInput) : base(parent, alignment, null, null)
    {
        IsInput = isInput;
    }

    public bool IsInput { get; set; }

    public override bool CanAttachTo(PortModel port)
    {
        // Checks for same-node/port attachements
        if (!base.CanAttachTo(port))
            return false;

        // Only able to attach to the same port type
        if (!(port is HelixPort cp))
            return false;

        return IsInput != cp.IsInput;
    }
}