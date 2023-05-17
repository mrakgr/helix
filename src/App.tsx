import {MutableRefObject, useCallback, useEffect, useRef, useState} from 'react';
import ReactFlow, {addEdge, useEdgesState, useNodesState} from 'reactflow';
import 'reactflow/dist/style.css';

const initialNodes = [
    {
        id: '1',
        data: {label: 'Node 1'},
        position: {x: 150, y: 0},
    },
    {
        id: '2',
        data: {label: 'Node 2'},
        position: {x: 0, y: 150},
    },
    {
        id: '3',
        data: {label: 'Node 3'},
        position: {x: 300, y: 150},
    },
];

const initialEdges = [
    {id: 'e1-2', source: '1', target: '2'},
    {id: 'e1-3', source: '1', target: '3'},
];

interface ContextMenuState {
    is_visible: boolean
    x: number
    y: number
}

interface ContextMenuDispatch {
    close: () => void
}

const initContextMenuState: ContextMenuState = {
    is_visible: false,
    x: 0,
    y: 0,
}

function closeOnClickedOutside(ref : MutableRefObject<HTMLElement | null>, close : () => void) {
    useEffect(() => {
        const listener = (ev : MouseEvent) => {
            const el = ref.current
            if (el && !(ev.target instanceof Node && el.contains(ev.target))) {
                close()
            }
        }
        document.addEventListener('click',listener)
        return () => {
            document.removeEventListener('click',listener)
        }
    }, [ref, close])
}

function ContextMenu(q: ContextMenuState & ContextMenuDispatch) {
    const ref : MutableRefObject<HTMLDivElement | null> = useRef(null)
    closeOnClickedOutside(ref, q.close) 
    if (q.is_visible) {
        return (
            <div 
                ref={ref}
                style={{left: q.x, top: q.y, backgroundColor: 'red', position: 'absolute', width: '300px', height: '300px'}}>
                This is the context menu.
                1...
                2...
                3...
            </div>
        )
    } else {
        return <></>
    }
}

function Flow() {
    const [nodes, setNodes, onNodesChange] = useNodesState(initialNodes);
    const [edges, setEdges, onEdgesChange] = useEdgesState(initialEdges);
    const [contextMenuState, setContextMenuState] = useState(initContextMenuState)

    const onConnect = useCallback(
        (connection: any) => setEdges((eds) => addEdge(connection, eds)),
        [setEdges]
    );
    
    function close() { setContextMenuState(x => { return { ...x, is_visible: false } }) }
    return (
        <div style={{width: 'inherit', height: 'inherit', position: 'relative'}}>
            <ReactFlow
                nodes={nodes}
                edges={edges}
                onNodesChange={onNodesChange}
                onEdgesChange={onEdgesChange}
                onConnect={onConnect}
                onContextMenu={ev => {
                    ev.preventDefault()
                    setContextMenuState({is_visible: true, x: ev.pageX, y: ev.pageY})
                }}
            />
            <ContextMenu {...contextMenuState } close={close} />
        </div>
    );
}

export default Flow;
