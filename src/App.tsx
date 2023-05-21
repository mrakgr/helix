/* eslint-disable @typescript-eslint/no-unused-vars */
import { FunctionComponent, useCallback, useMemo, useState } from 'react';
import ReactFlow, { addEdge, useEdgesState, useNodesState, Node, Edge, XYPosition, Connection, NodeProps, useReactFlow, ReactFlowInstance } from 'reactflow';
import 'reactflow/dist/style.css';
import { ContextMenu, ContextMenuDispatch, ContextMenuState } from './ContextMenu';
import { CompilationNode, CompilationOutputNode, HelixNode, HelixNodeTypeDefinitions, TextNode, TextNodeData } from './Nodes';
import { initialEdges, initialNodes, manager } from './NodeManager';

const initContextMenuState: ContextMenuState = {
    is_visible: false,
    x: 0,
    y: 0,
}

const nodesTypes: HelixNodeTypeDefinitions = {
    Text: TextNode,
    CompilationOutput: CompilationOutputNode,
    Compilation: CompilationNode,
}

function assertNever(x: never): never {
  throw new Error("Unexpected object: " + x);
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
            addNode: (position, type) => {
                position = reactFlowInstance.project(position) // TODO: This is not how the drag and drop example does it.
                let n: HelixNode
                switch (type) {
                    case 'Text':
                        n = manager.createText({}, position)
                        break;
                    case 'Compilation':
                        n = manager.createCompilation({}, position)
                        break;
                    case 'CompilationOutput':
                        n = manager.createCompilationOutput({}, position)
                        break;
                    default:
                        assertNever(type)
                }
                reactFlowInstance.addNodes(n)
            }
        };
    }
}

export default App;