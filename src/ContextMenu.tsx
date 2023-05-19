import { MutableRefObject, useEffect, useRef } from 'react';
import { XYPosition } from 'reactflow';

export interface ContextMenuState {
    is_visible: boolean;
    x: number;
    y: number;
}

export interface ContextMenuDispatch {
    close: () => void;
    addNode: (position: XYPosition, type: string) => void;
}

const useFocus = <T extends HTMLElement>(): [MutableRefObject<T | null>, () => void] => {
    const htmlElRef: MutableRefObject<T | null> = useRef(null);
    const setFocus = () => { htmlElRef.current && htmlElRef.current.focus(); };

    return [htmlElRef, setFocus];
};

function useCloseOnClickedOutside(ref: MutableRefObject<HTMLElement | null>, close: () => void) {
    useEffect(() => {
        const listener = (ev: MouseEvent) => {
            const el = ref.current;
            if (el && !(ev.target instanceof Node && el.contains(ev.target))) {
                close();
            }
        };
        document.addEventListener('click', listener);
        return () => {
            document.removeEventListener('click', listener);
        };
    }, [ref, close]);
}

export function ContextMenu(q: ContextMenuState & ContextMenuDispatch) {
    const ref: MutableRefObject<HTMLUListElement | null> = useRef(null);
    useCloseOnClickedOutside(ref, q.close);
    const onClick = (type: string) => (ev: React.MouseEvent) => {
        q.addNode({ x: ev.pageX, y: ev.pageY }, type)
        q.close()
    }
    return q.is_visible ? (
        <ul className="menu menu-compact bg-base-300 w-56 absolute"
            ref={ref}
            style={{ left: q.x, top: q.y }}
        >
            <li className="menu-title">
                <span>Add Node</span>
            </li>
            {
            [
            "Text", "CompilationNode", "CompilationOutputNode"
            ].map(type => <li key={type}><a onClick={onClick(type)}>{type}</a></li>)
            }
        </ul>
    ) : <></>;
}
