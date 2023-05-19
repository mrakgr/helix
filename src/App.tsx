import { FunctionComponent, useCallback, useMemo, useState } from 'react';
import ReactFlow, { addEdge, useEdgesState, useNodesState, Node, Edge, XYPosition, Connection, NodeProps, useReactFlow } from 'reactflow';
import 'reactflow/dist/style.css';
import { ContextMenu, ContextMenuDispatch, ContextMenuState } from './ContextMenu';
import { TextNode } from './TextNode';

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
        data: { label: 'Node 1' },
        position: { x: 150, y: 0 },
        type: 'text'
    },
    // {
    //     data: { label: 'Node 2' },
    //     position: { x: 0, y: 150 },
    //     type: 'text'
    // },
    // {
    //     data: { label: 'Node 3' },
    //     position: { x: 300, y: 150 },
    //     type: 'text'
    // }
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

const nodesTypes = {
    text: TextNode
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
            />
            <ContextMenu {...contextMenuState} {...useMemo(createDispatch, [reactFlowInstance])} />
        </div>
    );

    function createDispatch(): ContextMenuDispatch {
        let custom_id = 0;
        return {
            close: () => { setContextMenuState(x => { return { ...x, is_visible: false }; }); },
            addTextNode: (position: XYPosition) => {
                const label = `Custom ${++custom_id}`;
                reactFlowInstance.addNodes(manager.tagNode({ data: { label }, position: reactFlowInstance.project(position), type: 'text' }))
            }
        };
    }
}

export default App;