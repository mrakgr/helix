import { FunctionComponent, useCallback, useMemo, useState } from 'react';
import ReactFlow, { addEdge, useEdgesState, useNodesState, Node, Edge, XYPosition, Connection, NodeProps, useReactFlow } from 'reactflow';
import 'reactflow/dist/style.css';
import { ContextMenu, ContextMenuDispatch, ContextMenuState } from './ContextMenu';
import { CompilationNode, CompilationNodeData, CompilationOutputNode, TextNode, TextNodeData } from './Nodes';

interface NodeData<T> {
    data: T
    position: XYPosition
    type?: string
}

class NodeManager {
    id = 0

    // in the future we'll extend these so it supports multiple handles.
    tagNode = <T,>(data: NodeData<T>): Node<T> => {
        return {
            ...data,
            id: `${++this.id}`
        }
    }
}

function connectNodes(n1: Node, n2: Node): Edge {
    return {
        id: `e${n1.id}-${n2.id}`,
        source: n1.id,
        target: n2.id
    }
}

const manager = new NodeManager()

const initialNodes = [
    {
        data: {},
        position: { x: 150, y: 0 },
        type: 'TextNode'
    },
].map(manager.tagNode) // Note: Typescript and partial application do not mix.

const initialEdges: Edge[] = [
    // connectNodes(initialNodes[0], initialNodes[1]),
    // connectNodes(initialNodes[0], initialNodes[2])
]

const initContextMenuState: ContextMenuState = {
    is_visible: false,
    x: 0,
    y: 0,
}

const nodesTypes = { // Sigh, it is not worth creating a circular dependency. I'll leave these here.
    TextNode: TextNode,
    CompilationNode: CompilationNode,
    CompilationOutputNode: CompilationOutputNode
}

function App() {
    const reactFlowInstance = useReactFlow()

    const [contextMenuState, setContextMenuState] = useState(initContextMenuState)

    return (
        <div style={{ width: 'inherit', height: 'inherit', position: 'relative' }}>
            <ReactFlow
                defaultNodes={initialNodes}
                defaultEdges={initialEdges}
                nodeTypes={nodesTypes}
                onContextMenu={ev => {
                    ev.preventDefault()
                    setContextMenuState({ is_visible: true, x: ev.pageX, y: ev.pageY })
                }}
                minZoom={1 / 100}
            />
            <ContextMenu {...contextMenuState} {...useMemo(createDispatch, [reactFlowInstance])} />
        </div>
    );

    function createDispatch(): ContextMenuDispatch {
        return {
            close: () => { setContextMenuState(x => { return { ...x, is_visible: false }; }); },
            contextNodes: [
                {
                    contextMenuName: "Text",
                    onAdd: (position) => {
                        const data: TextNodeData = { 
                            data: {} 
                            }
                        addNode(data, "TextNode", position);
                    }
                },
                {
                    contextMenuName: "Compilation",
                    onAdd: (position) => {
                        const data: CompilationNodeData = { 
                            data: { onCompile: () => { return } } 
                            }
                        addNode(data, "CompilationNode", position);
                    }
                }
            ]
        };

        function addNode<T>(data: T, type: string, position: XYPosition) {
            reactFlowInstance.addNodes(manager.tagNode({ data, position: reactFlowInstance.project(position), type }));
        }
    }
}

export default App;