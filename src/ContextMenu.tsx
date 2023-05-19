import { MutableRefObject, useEffect, useRef } from 'react';
import { XYPosition } from 'reactflow';

export interface ContextMenuState {
    is_visible: boolean;
    x: number;
    y: number;
}

export interface ContextMenuDispatch {
    close: () => void;
    addTextNode: (position: XYPosition) => void;
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
    return q.is_visible ? (
        <ul className="menu menu-compact bg-base-300 w-56 absolute"
            ref={ref}
            style={{ left: q.x, top: q.y }}
        >
            <li><a onClick={onClick}>Add Node</a></li>
        </ul>
    ) : <></>;

    function onClick(ev : React.MouseEvent) {
        q.addTextNode({ x: ev.pageX, y: ev.pageY })
        q.close()
    }
}
