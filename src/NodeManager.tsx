import { Edge, Node, XYPosition } from 'reactflow';
import { CompilationNodeData, CompilationOutputNodeData, HelixNode, TextNodeData } from './Nodes';

class NodeManager {
    private id = 0;
    private getTag = () => `${++this.id}`

    createText = (data: TextNodeData, position: XYPosition): HelixNode => ({id: this.getTag(), type: 'Text', data, position})
    createCompilation = (data: CompilationNodeData, position: XYPosition): HelixNode => ({id: this.getTag(), type: 'Compilation', data, position})
    createCompilationOutput = (data: CompilationOutputNodeData, position: XYPosition): HelixNode => ({id: this.getTag(), type: 'CompilationOutput', data, position})
}
export const manager = new NodeManager();
export const initialNodes: HelixNode[] = [
    manager.createText({}, {x:300, y:0})
]

export function connectNodes(n1: Node, n2: Node): Edge {
    return {
        id: `${n1.id}-${n2.id}`,
        source: n1.id,
        target: n2.id
    }
}

export const initialEdges: Edge[] = [
    // connectNodes(initialNodes[0], initialNodes[1]),
    // connectNodes(initialNodes[0], initialNodes[2])
]